# =============================================================================
# Bill discrimination over time: does it differ between the ANC and ARP?
#
# Input:  data/estimate_all_2groups_ar_vb.rds  (combined ANC+ARP idealstan model)
#         data/all_votes.rds                   (only to attach readable titles
#                                               to ARP items)
#         output/coalition/coalition_vote_bloc_detail.csv
#           (the coalition analysis's detail table -- a prerequisite: run
#            coalition_cohesion.R first)
#
# Output: output/discrimination/bill_discrimination_detail.csv   (main detail)
#         output/discrimination/coalition_discrimination_merged.csv (merged)
#         output/discrimination/tables/*.csv
#         output/discrimination/figures/*.png
#
# Discrimination values are read from the model object's already-computed
# @summary slot rather than by calling the idealstan::summary() S4 method --
# on this machine, idealstan 1.0 combined with posterior 1.7.0 makes
# summary(est, pars="items") throw an error (an internal posterior function,
# assert_valid_draws_format, was removed in the newer version). @summary is a
# table computed once at fit time and stored in the RDS; reading it directly
# never touches the broken method. See the explanation doc, section 1.
#
# See bill_discrimination_说明.md for the full walkthrough.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
})

# ---- Configuration ----------------------------------------------------------

IN_MODEL     <- "data/estimate_all_2groups_ar_vb.rds"
IN_VOTES     <- "data/all_votes.rds"
IN_COALITION <- "output/coalition/coalition_vote_bloc_detail.csv"

OUT_DIR      <- "output/discrimination"
OUT_DETAIL   <- file.path(OUT_DIR, "bill_discrimination_detail.csv")
OUT_MERGED   <- file.path(OUT_DIR, "coalition_discrimination_merged.csv")
OUT_TABLES   <- file.path(OUT_DIR, "tables")
OUT_FIGURES  <- file.path(OUT_DIR, "figures")

# Reuse the project's shared ggplot2 theme
suppressMessages(source("R_Scripts/Ggplot2_theme.R"))

# ANC/ARP legislature boundary, taken from run_bawsala_combine_session.R's own
# convention (change = law_date > 2014-12-02). This is a different date from
# the 2018 bloc-restructuring BREAK_DATE in the coalition script -- don't
# confuse the two.
LEGISLATURE_BREAK <- as.Date("2014-12-02")

# The 8 identification-constraint items hardcoded in id_estimate() (see
# run_bawsala_combine_session.R's restrict_ind_high / restrict_ind_low).
# Their discrimination is pinned near +/-0.98 for scale identification, not
# freely estimated -- every ranking or correlation below must exclude them.
ANCHOR_ITEM_IDS <- c(
  "554ced8712bdaa5df2537688", "569416d212bdaa5ee3796068",
  "59b287314f24d0311313bfff", "5ba234a14f24d03ba3d842a2",
  "5603199812bdaa20aa5b4907", "5c9cf4aa4f24d0572feb077c",
  "57a31e71cf44122088ceed2c", "5866afa7cf44121f3e63b001"
)

TOP_N <- 15                    # how many items per end of the ranking
MIN_SHARED_FOR_COR <- 30       # min shared votes before a bloc correlation is reported

dir.create(OUT_DIR,     recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIGURES, recursive = TRUE, showWarnings = FALSE)


# ---- Helper functions -------------------------------------------------------

hdr <- function(x) {
  cat("\n\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")
}

show <- function(df) print(as.data.frame(df), row.names = FALSE)

save_table <- function(df, name) {
  path <- file.path(OUT_TABLES, paste0(name, ".csv"))
  write_csv(df, path)
  cat("  [table written]", path, "\n")
}

save_figure <- function(plot, name, width = 8, height = 5) {
  path <- file.path(OUT_FIGURES, paste0(name, ".png"))
  ggsave(path, plot = plot, width = width, height = height, dpi = 200)
  cat("  [figure written]", path, "\n")
}


# ---- 0. Prerequisite check ---------------------------------------------------
# The merge step needs the coalition analysis's output. This is not a stale
# cache -- this script recomputes its own discrimination part from scratch
# every run; it just also consumes an already-run, already-verified upstream
# result, which is a normal pipeline dependency. Fail loudly if it's missing
# rather than silently using stale data or skipping the step.
if (!file.exists(IN_COALITION)) {
  stop("Cannot find ", IN_COALITION,
       " -- run R_Scripts/coalition_cohesion.R first")
}


# ---- 1. Load the model and extract discrimination ---------------------------

hdr("1. Load the model and extract discrimination")

cat("Loading", IN_MODEL, "(~1GB, takes a few seconds)...\n")
t0 <- Sys.time()
est <- readRDS(IN_MODEL)
cat("Loaded in", round(difftime(Sys.time(), t0, units = "secs")), "sec\n")

sm <- est@score_data@score_matrix
s  <- as.data.frame(est@summary)

# The discrimination parameter is sigma_reg_free[1..6664], whose count exactly
# matches the total item count; covariate_analysis.R also matches on this
# exact parameter name for what post_processing_arp_model.R calls
# "Non-Inflated Discrimination". Anchor the regex to "^sigma_reg_free\\[" so
# it doesn't also catch sigma_reg_full.
item_discrim <- s %>%
  filter(str_detect(variable, "^sigma_reg_free\\[")) %>%
  transmute(
    index                  = as.numeric(str_extract(variable, "[0-9]+")),
    discrimination         = mean,
    discrimination_median  = median,
    ci_width               = abs(upper - lower),
    rhat                   = rhat
  )

# index -> item_id: item_id in score_matrix is already a factor, and
# as.numeric() on it gives exactly the Stan array index -- identical to the
# approach in covariate_analysis.R:69-71.
group_ids <- distinct(select(sm, item_id)) %>%
  mutate(index = as.numeric(item_id))

item_discrim <- left_join(item_discrim, group_ids, by = "index")

stopifnot(
  nrow(item_discrim) == n_distinct(sm$item_id),
  sum(is.na(item_discrim$item_id)) == 0
)

# item_id -> date. time_id in score_matrix has already been rounded to the
# 10th of the month by the original modelling pipeline
# (run_bawsala_combine_session.R's day(law_date) <- 10, done to give the
# spline time-varying model a tidy monthly anchor). This has no effect on the
# yearly/monthly aggregation done here, but it does mean law_date below is NOT
# the true vote date -- it should not be cross-checked day-for-day against
# all_votes.rds / the coalition analysis's vote_date; a few days' difference
# is expected, not a bug.
#
# Four items (all from the ANC period) map to two different months each: the
# ANC-era item_id is the bill's title text rather than a hash ID, and these
# four titles happen to be identical even though they were two genuinely
# different roll calls about a month apart. Resolved by taking the earlier
# date; n_dates_resolved flags them for manual review.
item_dates <- sm %>%
  distinct(item_id, time_id) %>%
  group_by(item_id) %>%
  summarise(law_date = min(time_id), n_dates_resolved = n(), .groups = "drop")

item_discrim <- left_join(item_discrim, item_dates, by = "item_id") %>%
  mutate(
    abs_discrimination = abs(discrimination),
    legislature        = if_else(law_date > LEGISLATURE_BREAK, "ARP", "ANC"),
    is_anchor          = as.character(item_id) %in% ANCHOR_ITEM_IDS
  ) %>%
  select(item_id, legislature, law_date, n_dates_resolved,
         discrimination, abs_discrimination, discrimination_median,
         ci_width, rhat, is_anchor) %>%
  arrange(law_date)

rm(est, sm, s)  # the model object is large and no longer needed


# ---- 2. Attach readable titles (display only) --------------------------------
# ARP item_id values are hashes and need a join against all_votes.rds to be
# readable; ANC item_id values are already readable French text and need no
# lookup. Some law_title/vote_title values are literally the string "False"
# (procedural votes with no associated law, e.g. confidence votes) --
# prefer law_title and fall back to vote_title when it reads "False".
votes_titles <- readRDS(IN_VOTES) %>%
  as_tibble() %>%
  distinct(vote_id, law_title, vote_title) %>%
  mutate(bill_title = if_else(law_title == "False" | is.na(law_title),
                              vote_title, law_title))

item_discrim <- item_discrim %>%
  left_join(select(votes_titles, item_id = vote_id, bill_title), by = "item_id") %>%
  mutate(bill_title = if_else(is.na(bill_title), as.character(item_id), bill_title))


# ---- 3. Write the detail table -----------------------------------------------

write_csv(item_discrim, OUT_DETAIL)
cat("\nDetail table written to:", OUT_DETAIL, "  (", nrow(item_discrim), "rows )\n")


# ---- 4. Self-checks -----------------------------------------------------------

hdr("Self-checks")

chk_total  <- nrow(item_discrim) == 6664
chk_anchor <- sum(item_discrim$is_anchor) == 8
chk_split  <- all(table(item_discrim$legislature)[c("ANC", "ARP")] == c(1692, 4972))
chk_range  <- all(item_discrim$abs_discrimination >= 0 &
                    item_discrim$abs_discrimination <= 2)
chk_dup    <- sum(item_discrim$n_dates_resolved > 1) == 4

cat("Item count == 6664                  :", chk_total,  " (", nrow(item_discrim), ")\n")
cat("Exactly 8 anchor items               :", chk_anchor, " (", sum(item_discrim$is_anchor), ")\n")
cat("ANC=1692 / ARP=4972                 :", chk_split, "\n")
cat("abs_discrimination within [0, 2]    :", chk_range, "\n")
cat("Exactly 4 date-ambiguous items       :", chk_dup, " (", sum(item_discrim$n_dates_resolved > 1), ")\n")

if (!all(chk_total, chk_anchor, chk_split, chk_range, chk_dup)) {
  warning("Self-checks did not all pass; treat the statistics below with caution")
}


# ---- 5. Descriptives: data overview ------------------------------------------

hdr("1. Data overview")

cat("Total items:", nrow(item_discrim), "\n")
cat("Date range :", as.character(min(item_discrim$law_date)), "->",
    as.character(max(item_discrim$law_date)), "\n")
cat("By legislature:\n")
print(table(item_discrim$legislature))
cat("\nrhat convergence diagnostic summary (should sit near 1; a clear outlier\n")
cat("means that item's estimate is unstable):\n")
print(summary(item_discrim$rhat))


# ---- 6. Descriptives: discrimination distribution, ANC vs ARP ----------------

hdr("2. Discrimination distribution, overall and ANC vs ARP")

discrim_by_legislature <- item_discrim %>%
  filter(!is_anchor) %>%
  group_by(legislature) %>%
  summarise(n      = n(),
            mean   = round(mean(abs_discrimination), 4),
            median = round(median(abs_discrimination), 4),
            sd     = round(sd(abs_discrimination), 4),
            p25    = round(quantile(abs_discrimination, .25), 4),
            p75    = round(quantile(abs_discrimination, .75), 4),
            .groups = "drop")

cat("(", sum(item_discrim$is_anchor), "anchor items excluded)\n\n")
discrim_by_legislature %>% show()
save_table(discrim_by_legislature, "01_discrimination_by_legislature")

p_dist <- item_discrim %>%
  filter(!is_anchor) %>%
  ggplot(aes(x = legislature, y = abs_discrimination, fill = legislature)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.size = 0.6) +
  labs(x = NULL, y = "|Discrimination|", fill = NULL,
       title = "Distribution of bill discrimination, ANC vs ARP") +
  my_theme +
  theme(legend.position = "none")

save_figure(p_dist, "01_discrimination_by_legislature", width = 6, height = 5)


# ---- 7. Descriptives: yearly trend (headline view) ---------------------------

hdr("3. Discrimination over time, by year (headline view)")

discrim_yearly <- item_discrim %>%
  filter(!is_anchor) %>%
  mutate(year = as.integer(format(law_date, "%Y"))) %>%
  group_by(year, legislature) %>%
  summarise(n = n(),
            mean_abs = round(mean(abs_discrimination), 4),
            sd_abs   = round(sd(abs_discrimination), 4),
            .groups = "drop") %>%
  arrange(year)

cat("Years with small n (e.g. 2012 has only 71 items) will be noisier;\n")
cat("read the mean alongside n:\n\n")
discrim_yearly %>% show()
save_table(discrim_yearly, "02_discrimination_yearly")

p_yearly <- ggplot(discrim_yearly, aes(x = year, y = mean_abs, fill = legislature)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = pmax(mean_abs - sd_abs, 0), ymax = mean_abs + sd_abs),
                width = 0.2) +
  geom_text(aes(label = n), vjust = -1.2, size = 3) +
  scale_x_continuous(breaks = discrim_yearly$year) +
  labs(x = NULL, y = "Mean |discrimination|", fill = "Legislature",
       title = "Average bill discrimination by year",
       caption = "Numbers above bars are item counts; error bars are +/- 1 SD") +
  my_theme

save_figure(p_yearly, "02_discrimination_yearly", width = 8, height = 5)


# ---- 8. Descriptives: monthly trend (supporting detail) ----------------------

hdr("4. Discrimination over time, by month (supporting detail)")

discrim_monthly <- item_discrim %>%
  filter(!is_anchor) %>%
  mutate(ym      = format(law_date, "%Y-%m"),
         ym_date = as.Date(paste0(ym, "-01"))) %>%
  group_by(legislature, ym, ym_date) %>%
  summarise(n = n(), mean_abs = round(mean(abs_discrimination), 4),
            .groups = "drop") %>%
  arrange(ym)

cat("Months with few items are noisy; read n alongside the mean.\n")
cat("ANC and ARP are separated by a real ~4-month changeover gap\n")
cat("(2014-10 to 2015-02) -- the line does not connect across it.\n\n")

save_table(select(discrim_monthly, -ym_date), "02_discrimination_monthly")

p_monthly <- ggplot(discrim_monthly,
                    aes(x = ym_date, y = mean_abs, group = legislature,
                        color = legislature)) +
  geom_line() +
  geom_point(aes(size = n), alpha = 0.7) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "Mean |discrimination| (monthly)",
       color = "Legislature", size = "Items that month",
       title = "Average bill discrimination by month") +
  my_theme

save_figure(p_monthly, "02_discrimination_monthly", width = 10, height = 5)


# ---- 9. Descriptives: most/least discriminating bills (anchors excluded) -----

hdr("5. Most and least discriminating bills (anchor items excluded)")

ranked <- item_discrim %>% filter(!is_anchor)

top_bills <- bind_rows(
  ranked %>% arrange(desc(abs_discrimination)) %>% head(TOP_N) %>%
    mutate(rank_group = "most_discriminating"),
  ranked %>% arrange(abs_discrimination) %>% head(TOP_N) %>%
    mutate(rank_group = "least_discriminating")
) %>%
  select(rank_group, item_id, bill_title, legislature, law_date,
         abs_discrimination, ci_width)

cat("Most discriminating:\n")
top_bills %>% filter(rank_group == "most_discriminating") %>%
  mutate(bill_title = substr(bill_title, 1, 60)) %>%
  select(-rank_group) %>% show()

cat("\nLeast discriminating:\n")
top_bills %>% filter(rank_group == "least_discriminating") %>%
  mutate(bill_title = substr(bill_title, 1, 60)) %>%
  select(-rank_group) %>% show()

save_table(top_bills, "03_top_bottom_discriminating_bills")


# ---- 10. Merge with the coalition analysis -----------------------------------

hdr("6. Relationship with agreement within the coalition")

coalition_detail <- read_csv(IN_COALITION, show_col_types = FALSE)

# The merge covers ARP only (the coalition analysis is ARP-only by design).
# Anchor items are excluded before merging so their 8 artificially pinned
# discrimination values don't contaminate the correlation analysis.
# Important: coalition_detail is NOT deduplicated here -- keeping its native
# vote x bloc grain is what lets this support both "coalition-wide agreement
# vs. discrimination" and "per-bloc agreement vs. discrimination" below.
arp_discrim <- item_discrim %>%
  filter(legislature == "ARP", !is_anchor) %>%
  transmute(vote_id = as.character(item_id), abs_discrimination)

merged <- coalition_detail %>%
  inner_join(arp_discrim, by = "vote_id")

cat("Coalition detail table rows:", nrow(coalition_detail), "\n")
cat("Rows matched to a discrimination value:", nrow(merged),
    "(unmatched rows are mostly votes tied to anchor items, as expected)\n\n")

write_csv(merged, OUT_MERGED)
cat("Merged table written to:", OUT_MERGED, "\n")

# 6.1 Coalition-wide agreement vs. discrimination
overall_pairs <- merged %>% distinct(vote_id, pct_agree_within_coalition, abs_discrimination)

cor_pearson  <- cor(overall_pairs$pct_agree_within_coalition,
                    overall_pairs$abs_discrimination, use = "complete.obs")
cor_spearman <- cor(overall_pairs$pct_agree_within_coalition,
                    overall_pairs$abs_discrimination, method = "spearman",
                    use = "complete.obs")

cat("\nAgreement within coalition vs. discrimination (n =", nrow(overall_pairs), "):\n")
cat("  Pearson r  =", round(cor_pearson, 3), "\n")
cat("  Spearman r =", round(cor_spearman, 3),
    " (agreement is heavily right-skewed, most votes near 100%; Spearman is more robust)\n")

save_table(
  tibble(n = nrow(overall_pairs), pearson_r = round(cor_pearson, 4),
         spearman_r = round(cor_spearman, 4)),
  "04_coalition_discrimination_overall_cor"
)

p_overall <- ggplot(overall_pairs, aes(x = abs_discrimination, y = pct_agree_within_coalition)) +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_smooth(method = "loess", se = TRUE, color = "steelblue") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  labs(x = "|Discrimination|", y = "Agreement within coalition",
       title = "Coalition agreement vs. bill discrimination",
       caption = paste0("Pearson r = ", round(cor_pearson, 3),
                        ", Spearman r = ", round(cor_spearman, 3),
                        "  (n = ", nrow(overall_pairs), ")")) +
  my_theme

save_figure(p_overall, "04_coalition_discrimination_overall", width = 7, height = 5)

# 6.2 Per-bloc: agree_within_bloc vs. discrimination
cat("\nPer-bloc correlation between within-bloc agreement and discrimination",
    "\n(reported only where at least", MIN_SHARED_FOR_COR, "shared votes exist):\n")

bloc_cor <- merged %>%
  filter(n_present > 0) %>%
  group_by(bloc, status) %>%
  filter(n() >= MIN_SHARED_FOR_COR) %>%
  summarise(n = n(),
            pearson_r  = round(cor(pct_agree_within_bloc, abs_discrimination,
                                   use = "complete.obs"), 3),
            spearman_r = round(cor(pct_agree_within_bloc, abs_discrimination,
                                   method = "spearman", use = "complete.obs"), 3),
            .groups = "drop") %>%
  arrange(desc(abs(pearson_r)))

bloc_cor %>% show()
save_table(bloc_cor, "05_coalition_discrimination_by_bloc_cor")

p_by_bloc <- merged %>%
  filter(n_present > 0) %>%
  semi_join(bloc_cor, by = c("bloc", "status")) %>%
  ggplot(aes(x = abs_discrimination, y = pct_agree_within_bloc)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 0.6) +
  facet_wrap(~bloc) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  labs(x = "|Discrimination|", y = "Agreement within bloc",
       title = "Within-bloc agreement vs. bill discrimination, by bloc") +
  my_theme

save_figure(p_by_bloc, "05_coalition_discrimination_by_bloc", width = 10, height = 8)


cat("\n\nDone. Detail table: ", OUT_DETAIL, "\n", sep = "")
cat("Merged table: ", OUT_MERGED, "\n", sep = "")
cat("Tables directory: ", OUT_TABLES, "\n", sep = "")
cat("Figures directory: ", OUT_FIGURES, "\n", sep = "")
