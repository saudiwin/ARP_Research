# =============================================================================
# Coalition voting agreement in the ARP (Tunisian parliament, 2014-2019)
#
# Input:  data/all_votes.rds
# Output: output/coalition/coalition_vote_bloc_detail.csv       (detail table)
#         output/coalition/tables/*.csv                         (6 requested tables)
#         output/coalition/figures/*.png                        (matching charts)
#         all other descriptive statistics are still printed to the console
#
# The script runs from the raw data straight through to the results and
# recomputes everything on every run. No intermediate files are read back in,
# so there is no risk of silently analysing a stale intermediate table. Tables
# and figures are produced fresh on every run rather than by a staged
# pipeline, so a change in methodology is reflected everywhere after one rerun.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})

# ---- Configuration ----------------------------------------------------------

IN_FILE     <- "data/all_votes.rds"
OUT_DIR     <- "output/coalition"
OUT_DETAIL  <- file.path(OUT_DIR, "coalition_vote_bloc_detail.csv")
OUT_TABLES  <- file.path(OUT_DIR, "tables")
OUT_FIGURES <- file.path(OUT_DIR, "figures")

# Reuse the project's shared ggplot2 theme (my_theme) so figures match the
# house style used by the other scripts.
suppressMessages(source("R_Scripts/Ggplot2_theme.R"))

# The October 2018 restructuring of the bloc system: UPL dissolved entirely,
# Tahya Tounes (Coalition nationale) was created, and Nidaa Tounes moved into
# opposition. Votes before this date are P1, votes on or after it are P2.
# Set to the exact first sitting day in the data: UPL's last vote was
# 2018-07-28, parliament then broke for summer recess (no votes at all in
# August or September), and when it reconvened on 2018-10-02 the same MPs'
# mp_bloc_name already read Tahya Tounes. This marks the parliamentary bloc
# restructuring, not the 2018-11-12 cabinet reshuffle / new coalition taking
# office -- that is an executive-branch event with no footprint in
# mp_bloc_name at all (Nidaa/Tahya monthly headcounts move smoothly through
# mid-November with no discontinuity), so it isn't the right anchor for a
# P1/P2 split defined by parliamentary voting blocs.
BREAK_DATE <- as.Date("2018-10-02")

# Afek Tounes announced its withdrawal from the Carthage Agreement. Uses the
# announcement date itself (not when MPs actually left the bloc): the data
# shows Afek's bloc was still a full 10 members through 2018-02; the real
# attrition only starts in 2018-03 (10->9->7->1), but this date tracks the
# party's declared political position, not the later physical departures.
AFEK_EXIT  <- as.Date("2018-01-06")

# Thresholds for calling a vote "the coalition voted as a single bloc".
# Deliberately not fixed at one value; all three are reported as a
# sensitivity check.
THRESHOLDS <- c(0.80, 0.90, 0.95)

# Minimum sample sizes for the pairwise bloc agreement table
MIN_CAST_FOR_POSITION <- 3    # min yes/no votes for a bloc to have a position
MIN_SHARED_VOTES      <- 50   # min shared votes before a bloc pair is reported

dir.create(OUT_DIR,     recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

VOTE_TYPES <- c("yes", "no", "abstain", "absent_excused", "absent_unexcused")

# Key dates in the coalition's history, annotated consistently across charts;
# change them here and every figure picks it up.
EVENT_DATES  <- as.Date(c("2016-02-01", "2016-07-30", "2018-02-01", "2018-10-01"))
EVENT_LABELS <- c("Horra split", "Carthage Agreement", "Afek exit", "2018 restructuring")


# ---- Helper functions -------------------------------------------------------

# Modal label across three counts; returns NA on a tie or when all are zero.
modal_label <- function(n1, n2, n3, labels = c("yes", "no", "abstain")) {
  m     <- pmax(n1, n2, n3)
  ties  <- (n1 == m) + (n2 == m) + (n3 == m)
  out   <- ifelse(n1 == m, labels[1], ifelse(n2 == m, labels[2], labels[3]))
  out[ties > 1 | m == 0] <- NA_character_
  out
}

# Political status of a bloc on a given day. Defined as a function of date
# rather than a fixed membership list, because the governing coalition changed
# membership entirely in October 2018:
#   P1 = Essid's four-party grand coalition (Nidaa + Nahda + UPL + Afek)
#   P2 = Coalition nationale + Ennahdha + Machrouu Tounes
# Bloc label mapping applied upstream in create_data.R:
#   Coalition nationale -> "Tahya Tounes", Machrouu Tounes -> "Horra",
#   Ennahdha -> "Nahda"
bloc_status <- function(bloc, date) {
  case_when(
    bloc == "Nahda"                                   ~ "coalition",
    bloc == "Nidaa Tounes"     & date <  BREAK_DATE   ~ "coalition",
    bloc == "Nidaa Tounes"     & date >= BREAK_DATE   ~ "former_governing",
    bloc == "Union Patriotique Libre"                 ~ "coalition",
    bloc == "Afek Tounes"      & date <  AFEK_EXIT    ~ "coalition",
    bloc == "Afek Tounes"      & date >= AFEK_EXIT    ~ "opposition",
    bloc == "Horra"            & date <  BREAK_DATE   ~ "non_coalition",
    bloc == "Horra"            & date >= BREAK_DATE   ~ "coalition",
    bloc == "Tahya Tounes"                            ~ "coalition",
    bloc == "Front Populaire"                         ~ "opposition",
    grepl("^Social-D", bloc)                          ~ "opposition",
    bloc == "Aucun bloc"                              ~ "independent",
    TRUE                                              ~ NA_character_
  )
}

hdr <- function(x) {
  cat("\n\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")
}

show <- function(df) print(as.data.frame(df), row.names = FALSE)

# Write a data frame to output/coalition/tables/ as CSV, printing its path.
save_table <- function(df, name) {
  path <- file.path(OUT_TABLES, paste0(name, ".csv"))
  write_csv(df, path)
  cat("  [table written]", path, "\n")
}

# Write a ggplot object to output/coalition/figures/ as PNG, printing its path.
save_figure <- function(plot, name, width = 8, height = 5) {
  path <- file.path(OUT_FIGURES, paste0(name, ".png"))
  ggsave(path, plot = plot, width = width, height = height, dpi = 200)
  cat("  [figure written]", path, "\n")
}


# ---- 1. Load raw data; derive vote type, period and bloc status -------------

votes <- readRDS(IN_FILE) %>%
  as_tibble() %>%
  mutate(
    bloc   = as.character(mp_bloc_name),
    period = if_else(vote_date < BREAK_DATE,
                     "P1_2015-02..2018-09", "P2_2018-10..2019-08"),
    # Five mutually exclusive vote types. A non-NA vote_choice is equivalent to
    # event_presence == TRUE; absences are then split by whether they were
    # excused. The raw Al Bawsala data had a separate "excuse" category that
    # create_data.R collapsed into NA, but event_absence_justified preserves
    # it, so it is recovered here.
    vote_type = case_when(
      vote_choice == "YES"                        ~ "yes",
      vote_choice == "NO"                         ~ "no",
      vote_choice == "ABSTAIN"                    ~ "abstain",
      !event_presence &  event_absence_justified  ~ "absent_excused",
      !event_presence & !event_absence_justified  ~ "absent_unexcused"
    ),
    status = bloc_status(bloc, vote_date)
  )

# Completeness check: every row must fall into exactly one of the five vote
# types, and every bloc must have a defined political status. A failure here
# means bloc_status() is missing a bloc name; fix the rule rather than
# skipping, otherwise that bloc's MPs are silently dropped.
stopifnot(
  !any(is.na(votes$vote_type)),
  !any(is.na(votes$status)),
  all(votes$vote_type %in% VOTE_TYPES)
)


# ---- 2. Aggregate to the [vote x bloc] detail table -------------------------

vb <- votes %>%
  count(vote_id, law_id, law_title, vote_title, vote_date, period,
        bloc, status, vote_type, name = "n") %>%
  pivot_wider(names_from = vote_type, values_from = n, values_fill = 0)

# A vote type may never occur in the data; pad it with zeros to keep the
# column set stable.
for (v in VOTE_TYPES) if (!v %in% names(vb)) vb[[v]] <- 0L

vb <- vb %>%
  rename(n_yes              = yes,
         n_no               = no,
         n_abstain          = abstain,
         n_absent_excused   = absent_excused,
         n_absent_unexcused = absent_unexcused) %>%
  mutate(
    n_members = n_yes + n_no + n_abstain + n_absent_excused + n_absent_unexcused,
    n_present = n_yes + n_no + n_abstain,   # denominator, primary definition
    n_cast    = n_yes + n_no                # denominator, secondary definition
  )


# ---- 3. Measure one: agreement within a bloc --------------------------------
#
# Share of a bloc's MPs present who voted the bloc's own modal position.
# This is what the literature calls party cohesion.
#
#   Primary (abstentions in the denominator):
#     agree_within_bloc = max(n_yes, n_no, n_abstain) / n_present
#     Range [1/3, 1] -- the mode of THREE categories, so the lower bound is
#     1/3, not 1/2. (e.g. 4 yes / 3 no / 3 abstain -> 4/10 = 0.4)
#
#   Secondary (yes/no only):
#     agree_within_bloc_yn = max(n_yes, n_no) / n_cast
#     Range [1/2, 1]
#
# Both are kept: inside a coalition an abstention is often precisely the
# "unwilling to oppose openly" compromise signal, so whether it counts as
# disagreement or is excluded from the denominator is a substantive choice.

vb <- vb %>%
  mutate(
    bloc_majority            = modal_label(n_yes, n_no, n_abstain),
    n_agree_within_bloc      = pmax(n_yes, n_no, n_abstain),
    pct_agree_within_bloc    = if_else(n_present > 0,
                                       n_agree_within_bloc / n_present, NA_real_),

    bloc_majority_yn         = if_else(n_yes == n_no, NA_character_,
                                       if_else(n_yes > n_no, "yes", "no")),
    pct_agree_within_bloc_yn = if_else(n_cast > 0,
                                       pmax(n_yes, n_no) / n_cast, NA_real_)
  )


# ---- 4. Measure two: agreement within the coalition (+ did the bloc side with it)
#
# Measure two -- agreement within the coalition: pool every MP holding
#   coalition status on that date into a single group C and apply exactly the
#   same formula as measure one. Same operation, different group.
#     agree_within_coalition = max(n_yes_C, n_no_C, n_abstain_C) / n_present_C
#     Range [1/3, 1]
#
# follows_coalition: did this bloc's majority position match the coalition's
#   majority position? (boolean)
#
#   This slot previously held a continuous "share following the coalition",
#   n_M_bloc / n_present_bloc. Measured against the data, it was numerically
#   identical to agree_within_bloc in 95.5% of rows -- whenever the bloc's
#   majority IS the coalition's majority, the two share a numerator and a
#   denominator. The informative cases are the remaining 4.5% (797 rows),
#   which are exactly the rows where follows_coalition == FALSE. So only the
#   boolean is kept: FALSE pinpoints coalition-breakdown events, and where it
#   is TRUE the magnitude is already given by agree_within_bloc.
#
#   If the continuous rate is needed, it is recoverable from the columns in
#   this table: n_M_bloc / n_present, taking n_M_bloc by coalition_majority.

# Pool every MP holding coalition status on that date to get the coalition's
# own position and its internal agreement rate on the vote.
coalition_pos <- votes %>%
  filter(status == "coalition") %>%
  count(vote_id, vote_type, name = "n") %>%
  pivot_wider(names_from = vote_type, values_from = n, values_fill = 0)

for (v in VOTE_TYPES) if (!v %in% names(coalition_pos)) coalition_pos[[v]] <- 0L

coalition_pos <- coalition_pos %>%
  transmute(
    vote_id,
    coal_yes     = yes,
    coal_no      = no,
    coal_abstain = abstain,
    coal_present = yes + no + abstain,
    coalition_majority     = modal_label(yes, no, abstain),
    # Cohesion of the coalition treated as a single group
    pct_agree_within_coalition = if_else(coal_present > 0,
                                     pmax(yes, no, abstain) / coal_present,
                                     NA_real_)
  )

# Back at bloc level: did this bloc's majority match the coalition's?
# (modal_label returns NA on a tie, so a tie on either side leaves this NA
# rather than forcing an arbitrary verdict)
vb <- vb %>%
  left_join(select(coalition_pos, vote_id, coalition_majority,
                   pct_agree_within_coalition),
            by = "vote_id") %>%
  mutate(follows_coalition = bloc_majority == coalition_majority) %>%
  arrange(vote_date, vote_id, bloc)


# ---- 5. Write the detail table ----------------------------------------------

write_csv(vb, OUT_DETAIL)
cat("Detail table written to:", OUT_DETAIL, "  (", nrow(vb), "rows )\n")


# ---- 6. Self-checks ---------------------------------------------------------

hdr("Self-checks")

chk_total <- sum(vb$n_members) == nrow(votes)

# The primary measure is the mode of three categories, so its lower bound is
# 1/3, not 1/2.
chk_range <- all(is.na(vb$pct_agree_within_bloc) |
                   (vb$pct_agree_within_bloc >= 1/3 - 1e-9 &
                      vb$pct_agree_within_bloc <= 1 + 1e-9))

# The secondary measure has only two categories, so 1/2 is the bound there.
chk_range_yn <- all(is.na(vb$pct_agree_within_bloc_yn) |
                      (vb$pct_agree_within_bloc_yn >= 0.5 - 1e-9 &
                         vb$pct_agree_within_bloc_yn <= 1 + 1e-9))

chk_items <- n_distinct(vb$vote_id) == n_distinct(votes$vote_id)

cat("Five vote-type counts sum to input rows :", chk_total,
    " (", sum(vb$n_members), "vs", nrow(votes), ")\n")
cat("pct_agree_within_bloc    in [1/3, 1]    :", chk_range, "\n")
cat("pct_agree_within_bloc_yn in [1/2, 1]    :", chk_range_yn, "\n")
cat("Vote count preserved                    :", chk_items,
    " (", n_distinct(vb$vote_id), ")\n")

if (!all(chk_total, chk_range, chk_range_yn, chk_items)) {
  warning("Self-checks did not all pass; treat the statistics below with caution")
}


# ---- 7. Descriptives: data overview -----------------------------------------

hdr("1. Data overview")

cat("Votes  (vote_id) :", n_distinct(votes$vote_id), "\n")
cat("Laws   (law_id)  :", n_distinct(votes$law_id), "\n")
cat("MPs    (mp_id)   :", n_distinct(votes$mp_id), "\n")
cat("Date range       :", as.character(min(votes$vote_date)), "->",
    as.character(max(votes$vote_date)), "\n")

cat("\nDistribution of votes per law:\n")
print(summary(as.numeric(table(distinct(votes, vote_id, law_id)$law_id))))

cat("\nTop 5 laws by number of votes (a single law can dominate the month it\n")
cat("falls in, which matters when reading the monthly series):\n")
votes %>%
  distinct(vote_id, law_id, law_title) %>%
  count(law_id, law_title, name = "n_votes") %>%
  arrange(desc(n_votes)) %>%
  head(5) %>%
  mutate(law_title = substr(law_title, 1, 70)) %>%
  show()


# ---- 8. Descriptives: vote type distribution --------------------------------

hdr("2. Vote type distribution (including presence and abstention)")

cat("Whole term:\n")
votes %>%
  count(vote_type, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  arrange(desc(n)) %>%
  show()

cat("\nBy period (columns are percentages within that period):\n")
votes %>%
  count(period, vote_type, name = "n") %>%
  group_by(period) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = period, values_from = pct) %>%
  show()

cat("\nWithin the coalition only, by period (did the restructuring change\n")
cat("attendance and abstention behaviour?):\n")
coalition_vote_type <- votes %>%
  filter(status == "coalition") %>%
  count(period, vote_type, name = "n") %>%
  group_by(period) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  ungroup()

coalition_vote_type %>%
  select(-n) %>%
  pivot_wider(names_from = period, values_from = pct) %>%
  show()

save_table(coalition_vote_type, "01_coalition_vote_type_by_period")

p_vote_type <- coalition_vote_type %>%
  mutate(vote_type = factor(vote_type, levels = VOTE_TYPES,
                            labels = c("Yes", "No", "Abstain",
                                      "Absent (excused)", "Absent (unexcused)"))) %>%
  ggplot(aes(x = period, y = pct, fill = vote_type)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = "Share (%)", fill = "Vote type",
       title = "Coalition vote type distribution, by period") +
  my_theme

save_figure(p_vote_type, "01_coalition_vote_type_by_period", width = 7, height = 5)


# ---- 9. Descriptives: coalition composition before/after 2018 ---------------

hdr("3. Coalition composition before and after the 2018 restructuring")

cat("Political status and MP count of each bloc, by period:\n")
composition_by_bloc <- votes %>%
  distinct(period, bloc, status, mp_id) %>%
  count(period, bloc, status, name = "n_mp")

composition_by_bloc %>%
  pivot_wider(names_from = period, values_from = n_mp, values_fill = 0) %>%
  arrange(desc(status), bloc) %>%
  show()

save_table(composition_by_bloc, "02_composition_by_bloc_period")

cat("\nMP counts aggregated by political status:\n")
composition_by_status <- votes %>%
  distinct(period, status, mp_id) %>%
  count(period, status, name = "n_mp")

composition_by_status %>%
  pivot_wider(names_from = period, values_from = n_mp, values_fill = 0) %>%
  show()

save_table(composition_by_status, "02_composition_by_status_period")

cat("\nSize of each political status group by month (coalition size is one\n")
cat("of these lines):\n")
composition_by_month <- votes %>%
  mutate(ym = format(vote_date, "%Y-%m"),
         ym_date = as.Date(paste0(ym, "-15"))) %>%   # mid-month date, for plotting only
  distinct(ym, ym_date, status, mp_id) %>%
  count(ym, ym_date, status, name = "n_mp") %>%
  arrange(ym)

composition_by_month %>% select(-ym_date) %>% show()

save_table(select(composition_by_month, -ym_date), "02_composition_by_month")

status_labels <- c(coalition = "Coalition", opposition = "Opposition",
                   former_governing = "Former governing (P2 Nidaa)",
                   non_coalition = "Non-coalition (P1 Horra)",
                   independent = "Independent")

p_composition <- composition_by_month %>%
  mutate(status_l = status_labels[status]) %>%
  ggplot(aes(x = ym_date, y = n_mp, color = status_l)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  geom_vline(xintercept = EVENT_DATES, linetype = "dashed", color = "grey60") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(x = NULL, y = "Number of MPs", color = "Political status",
       title = "Size of each political camp, by month",
       caption = paste0("Dashed lines, left to right: ",
                        paste(paste0(EVENT_LABELS, " (",
                                    format(EVENT_DATES, "%Y-%m"), ")"),
                              collapse = " / "))) +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_figure(p_composition, "02_composition_by_month", width = 10, height = 5.5)


# ---- 10. Descriptives: MP movement at the restructuring ---------------------

hdr("4. October 2018 restructuring: where MPs moved")

last_before <- votes %>%
  filter(vote_date < BREAK_DATE) %>%
  group_by(mp_id) %>%
  filter(vote_date == max(vote_date)) %>%
  ungroup() %>%
  distinct(mp_id, bloc_before = bloc)

first_after <- votes %>%
  filter(vote_date >= BREAK_DATE) %>%
  group_by(mp_id) %>%
  filter(vote_date == min(vote_date)) %>%
  ungroup() %>%
  distinct(mp_id, bloc_after = bloc)

moves <- inner_join(last_before, first_after, by = "mp_id")

cat("Transition matrix (rows = before, columns = after):\n")
transition_long <- moves %>% count(bloc_before, bloc_after, name = "n")

transition_long %>%
  pivot_wider(names_from = bloc_after, values_from = n, values_fill = 0) %>%
  show()

# Long format is more reusable for saving (a wide table's columns depend on
# which blocs exist, a long table's columns are fixed).
save_table(transition_long, "03_bloc_transition_matrix")

cat("\nWhere the newly formed Tahya Tounes drew its members from:\n")
tahya_sources <- moves %>%
  filter(bloc_after == "Tahya Tounes") %>%
  count(bloc_before, name = "n") %>%
  arrange(desc(n))

tahya_sources %>% show()

save_table(tahya_sources, "03_tahya_tounes_sources")


# ---- 11. Descriptives: agreement rates --------------------------------------

hdr("5. Agreement within the coalition (headline result)")

vote_level <- vb %>%
  distinct(vote_id, vote_date, period, pct_agree_within_coalition) %>%
  filter(!is.na(pct_agree_within_coalition))

cat("Agreement within the coalition, by period:\n")
agreement_by_period <- vote_level %>%
  group_by(period) %>%
  summarise(n_votes = n(),
            mean    = round(mean(pct_agree_within_coalition), 4),
            median  = round(median(pct_agree_within_coalition), 4),
            p25     = round(quantile(pct_agree_within_coalition, .25), 4),
            p75     = round(quantile(pct_agree_within_coalition, .75), 4),
            .groups = "drop")

agreement_by_period %>% show()
save_table(agreement_by_period, "04_coalition_agreement_by_period")

cat("\nPer-bloc comparison, by period:\n")
cat("  agree_within_bloc  = mean share voting with their own bloc's majority\n")
cat("                       (party discipline), range [1/3, 1]\n")
cat("  pct_with_coalition = share of votes where the bloc's majority matched\n")
cat("                       the coalition's majority\n")
cat("  n_against          = number of votes against the coalition majority\n")
cat("                       (coalition-breakdown events)\n")
cat("  The last two are only meaningful for blocs holding coalition status\n\n")
bloc_comparison <- vb %>%
  filter(n_present > 0) %>%
  group_by(period, bloc, status) %>%
  summarise(n_votes            = n(),
            agree_within_bloc  = round(mean(pct_agree_within_bloc, na.rm = TRUE), 4),
            pct_with_coalition = round(mean(follows_coalition, na.rm = TRUE), 4),
            n_against          = sum(!follows_coalition, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(period, desc(pct_with_coalition))

bloc_comparison %>% show()
save_table(bloc_comparison, "04_bloc_comparison_by_period")

cat("\nNote: a bloc with high agree_within_bloc but low pct_with_coalition is\n")
cat("      internally united while voting against the coalition line. That is\n")
cat("      coalition breakdown, not a collapse of party discipline.\n")

# n_against above is computed for every bloc, but it only means "deviation"
# for blocs that actually belong to the coalition. An opposition/independent
# bloc voting against the coalition majority isn't "defecting" -- it never
# joined in the first place, it's just taking a different position. Filter to
# status == coalition to answer "how many times did coalition members vote
# against the coalition" cleanly.
cat("\nHow many times each coalition-member bloc voted against the coalition\n")
cat("(status == coalition only; n_against for the other blocs is in the table\n")
cat("above too, but that's 'voting opposite the coalition', not 'defecting',\n")
cat("a different thing, so it's excluded here):\n")
coalition_deviation <- bloc_comparison %>%
  filter(status == "coalition") %>%
  transmute(period, bloc, n_votes,
            n_against,
            pct_against = round(100 * n_against / n_votes, 1)) %>%
  arrange(period, desc(n_against))

coalition_deviation %>% show()
save_table(coalition_deviation, "04_coalition_deviation_events")

# Faceted by period, each panel sorted by its own n_against (scales="free_y"),
# so Nahda appearing in both periods doesn't tangle the two orderings together
p_deviation <- ggplot(coalition_deviation,
                      aes(x = reorder(bloc, n_against), y = n_against)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_text(aes(label = paste0(n_against, " (", pct_against, "%)")),
            hjust = -0.05, size = 3) +
  facet_wrap(~period, scales = "free_y") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(x = NULL, y = "Number of votes against the coalition majority",
       title = "How often each coalition member voted against the coalition") +
  my_theme

save_figure(p_deviation, "04_coalition_deviation_events", width = 9, height = 4.5)

# Monthly series: computed once here and reused both for this section's
# headline chart and for the console printout in section 14, instead of
# writing the same group_by twice.
monthly_agreement <- vote_level %>%
  mutate(ym      = format(vote_date, "%Y-%m"),
         ym_date = as.Date(paste0(ym, "-15"))) %>%   # mid-month date, for plotting only
  group_by(period, ym, ym_date) %>%
  summarise(n_votes    = n(),
            mean_agree = round(mean(pct_agree_within_coalition), 4),
            sd_agree   = round(sd(pct_agree_within_coalition), 4),
            .groups    = "drop") %>%
  arrange(ym)

save_table(select(monthly_agreement, -ym_date), "04_coalition_agreement_monthly")

# Point size is weighted by that month's vote count, so thin-sample months
# are visibly small dots. group = period breaks the line at the P1/P2
# boundary instead of drawing one continuous curve across the break.
p_monthly_agreement <- ggplot(monthly_agreement,
                              aes(x = ym_date, y = mean_agree, group = period)) +
  geom_line(color = "steelblue") +
  geom_point(aes(size = n_votes), color = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = EVENT_DATES, linetype = "dashed", color = "grey60") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  scale_y_continuous(limits = c(0, 1),
                     labels = function(x) paste0(round(100 * x), "%")) +
  labs(x = NULL, y = "Agreement within coalition (monthly mean)",
       size = "Votes that month",
       title = "Agreement within the coalition, by month",
       caption = paste0("Dashed lines, left to right: ",
                        paste(paste0(EVENT_LABELS, " (",
                                    format(EVENT_DATES, "%Y-%m"), ")"),
                              collapse = " / "),
                        "\nP1/P2 are separated by a structural break from the ",
                        "bloc restructuring; the line does not connect across it")) +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_figure(p_monthly_agreement, "04_coalition_agreement_monthly",
           width = 10, height = 5.5)


# ---- 12. Descriptives: pairwise bloc agreement ------------------------------

hdr("6. Pairwise bloc agreement (on yes/no majority positions)")

bloc_pos <- vb %>%
  filter(n_cast >= MIN_CAST_FOR_POSITION, !is.na(bloc_majority_yn)) %>%
  select(vote_id, period, bloc, pos = bloc_majority_yn)

pair_agree <- bloc_pos %>%
  inner_join(bloc_pos, by = c("vote_id", "period"),
             relationship = "many-to-many") %>%
  filter(bloc.x < bloc.y) %>%
  group_by(period, bloc_a = bloc.x, bloc_b = bloc.y) %>%
  summarise(n_shared_votes = n(),
            pct_agree      = round(100 * mean(pos.x == pos.y), 1),
            .groups = "drop") %>%
  filter(n_shared_votes >= MIN_SHARED_VOTES) %>%
  arrange(period, desc(pct_agree))

for (p in sort(unique(pair_agree$period))) {
  cat("\n---", p, "---\n")
  pair_agree %>% filter(period == p) %>% select(-period) %>% show()
}

save_table(pair_agree, "05_pairwise_bloc_agreement")

# A heatmap needs a symmetric matrix (both A-B and B-A), plus a diagonal
# (self-agreement is trivially 100%) just so the panel reads as a complete
# square -- no extra computation is implied by this.
pair_sym <- bind_rows(
  pair_agree,
  pair_agree %>% rename(bloc_a = bloc_b, bloc_b = bloc_a)
)
diag_rows <- pair_agree %>%
  distinct(period, bloc_a) %>%
  bind_rows(pair_agree %>% distinct(period, bloc_b) %>% rename(bloc_a = bloc_b)) %>%
  distinct(period, bloc_a) %>%
  mutate(bloc_b = bloc_a, n_shared_votes = NA_integer_, pct_agree = 100)
pair_sym <- bind_rows(pair_sym, diag_rows)

p_pairwise <- ggplot(pair_sym, aes(x = bloc_a, y = bloc_b, fill = pct_agree)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(pct_agree)), size = 3) +
  facet_wrap(~period, scales = "free", ncol = 1) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                       midpoint = 75, limits = c(0, 100)) +
  labs(x = NULL, y = NULL, fill = "Agreement (%)",
       title = "Pairwise bloc agreement (on yes/no majority positions)") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_figure(p_pairwise, "05_pairwise_bloc_agreement", width = 8, height = 11)


# ---- 13. Descriptives: how often the coalition voted as one bloc ------------

hdr("7. How often the coalition voted as a single bloc (threshold sensitivity)")

cat("Counted by vote:\n")
threshold_by_vote <- lapply(THRESHOLDS, function(th) {
  vote_level %>%
    group_by(period) %>%
    summarise(threshold = th,
              n_votes   = n(),
              n_unified = sum(pct_agree_within_coalition >= th),
              pct       = round(100 * mean(pct_agree_within_coalition >= th), 1),
              .groups   = "drop")
}) %>%
  bind_rows() %>%
  arrange(period, threshold)

threshold_by_vote %>% show()
save_table(threshold_by_vote, "06_threshold_frequency_by_vote")

cat("\nCounted by law (agreement averaged across the law's votes, then compared\n")
cat("against the threshold):\n")
law_level <- vb %>%
  distinct(vote_id, law_id, period, pct_agree_within_coalition) %>%
  filter(!is.na(pct_agree_within_coalition)) %>%
  group_by(law_id, period) %>%
  summarise(n_votes_in_law = n(),
            law_agree   = mean(pct_agree_within_coalition),
            .groups = "drop")

threshold_by_law <- lapply(THRESHOLDS, function(th) {
  law_level %>%
    group_by(period) %>%
    summarise(threshold = th,
              n_laws    = n(),
              n_unified = sum(law_agree >= th),
              pct       = round(100 * mean(law_agree >= th), 1),
              .groups   = "drop")
}) %>%
  bind_rows() %>%
  arrange(period, threshold)

threshold_by_law %>% show()
save_table(threshold_by_law, "06_threshold_frequency_by_law")

threshold_combined <- bind_rows(
  mutate(threshold_by_vote, level = "By vote"),
  mutate(threshold_by_law, level = "By law")
)

p_threshold <- ggplot(threshold_combined,
                      aes(x = factor(threshold), y = pct, fill = period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_dodge(width = 0.7), vjust = -0.4, size = 3) +
  facet_wrap(~level) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Threshold", y = "Share meeting the threshold (%)", fill = "Period",
       title = "How often the coalition voted as a single bloc (threshold sensitivity)") +
  my_theme

save_figure(p_threshold, "06_threshold_frequency", width = 9, height = 5)


# ---- 14. Descriptives: monthly series ---------------------------------------

hdr("8. Agreement within the coalition, by month")

cat("Months with only a handful of votes are extremely noisy, so n_votes must\n")
cat("be read alongside the agreement figure. P1 and P2 are separated by a full\n")
cat("restructuring of the bloc system and should not be plotted as one\n")
cat("continuous series across the break.\n")
cat("(Shares the same monthly data as section 5's headline result, already\n")
cat(" saved as 04_coalition_agreement_monthly.csv, chart\n")
cat(" 04_coalition_agreement_monthly.png)\n\n")

monthly_agreement %>% select(-ym_date) %>% show()

cat("\n\nDone. Detail table: ", OUT_DETAIL, "\n", sep = "")
cat("Tables directory: ", OUT_TABLES, "\n", sep = "")
cat("Figures directory: ", OUT_FIGURES, "\n", sep = "")
