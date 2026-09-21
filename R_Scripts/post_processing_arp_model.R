require(idealstan)
require(bayesplot)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(forcats)
require(xtable)
require(textclean)
library(ggthemes)

### ANC-ARP Joint Model

# load fitted anc-arp model & analyze it

estimate_all <- readRDS("data/estimate_all_2groups_ar_vb.rds")

# load data

# need to load and edit first session

sess1 <- readr::read_csv('data/ANC_votes.csv') %>% 
  gather(key = id,value=outcome,-legis.names,-bloc) %>% 
  mutate(id=as.numeric(id),
         outcome=ifelse(is.na(outcome),'absent',outcome),
         outcome=factor(outcome,levels=c('contre','abstenu','pour','absent')))

sess1_billnames <- readxl::read_excel('data/ANC_votes_labels-HM-edits.xlsx') %>% 
  select(-`...2`)


sessc <- left_join(sess1,sess1_billnames) %>% 
  select(-id,
         legis_names=legis.names,
         law_unique=ANC_bill_names,
         law_date=Date) %>% 
  mutate(clean_votes=as.numeric(outcome),
         law_date=ymd(law_date)) %>% 
  select(-outcome) %>% 
  filter(!is.na(law_date))

day(sessc$law_date) <- 10

# load ARP data

  all_votes <- readRDS("data/all_votes.rds") %>% 
    mutate(vote_choice=na_if(vote_choice, "ABSTAIN"),
           mp_bloc_name=fct_relevel(factor(mp_bloc_name),"Nahda")) |> 
    rename(legis_names="mp_id",law_unique="vote_id",law_date="vote_date",
      clean_votes="vote_choice",bloc="mp_bloc_name") |> 
        mutate(clean_votes=as.numeric(clean_votes))
day(all_votes$law_date) <- 10

# combine sessions

group_id <- bind_rows(all_votes,sessc) %>% 
  mutate(faction=fct_collapse(bloc,
                           Islamists=c("Mouvement Nahdha",
                                       "Mouvement Ennahdha",
                                      "Nahda"),
                           Secularists=c("Afek Tounes et l'appel des tunisiens l'tranger",
                                         "AllA(C)geance A la Patrie",
                                         "Alliance D\xe9mocratique",
                                         "Afek Tounes",
                                         "Horra",
                                         "Tahya Tounes",
                                         "Social-Démocrate",
                                         "Nidaa Tounes",
                                         "Aucun bloc",
                                         "Bloc Al Horra du Mouvement Machrouu Tounes",
                                         "Bloc D\xe9mocrates",
                                         "Congr\xe8s Pour La R\xe9publique",
                                         "Ettakatol",
                                         "Alliance Démocratique",
                                         "Alliance Dmocratique",
                                         "Bloc National",
                                         "Bloc Social-Dmocrate",
                                         "Fid\xe9lit\xe9 \xe0 La R\xe9volution",
                                         "Front Populaire",
                                         "Independent",
                                         "Mouvement Nidaa Tounes",
                                         "Union Patriotique Libre",
                                         "Transition D\xe9mocratique")),
                                        clean_votes=na_if(clean_votes, clean_votes %in% c(2,4)),
                                      clean_votes=ifelse(clean_votes==1, 0, ifelse(clean_votes==3, 1,NA)))

# Need a covariate for beginning of ARP

group_id <- group_by(group_id,bloc) %>% 
  mutate(change=as.numeric(law_date>lubridate::mdy('12-2-2014')))

# strip out underscores from MP names

group_id <- mutate(group_id, legis_names=str_replace_all(legis_names, "_", " "))

# same MPs are spelled differently across the ANC and ARP source files
# (transliteration variants of the same name) -- relabel to a single
# canonical spelling so they aren't treated as distinct legislators

name_recode <- c(
  "Badreddine Abdelkefi" = "Badreddine Abdelkafi",
  "Meherzia Labidi"      = "Meherzia Laabidi",
  "Khmais Ksila"         = "Khemais Ksila",
  "Monia Ibrahim"        = "Monia Brahim",
  "Iyed Dahmani"         = "Iyad Dahmani",
  "Oussama Al Sghaier"   = "Oussama Al Saghir",
  "Bechir Lazzem"        = "Bechir Ellazzem",
  "Walid Bennani"        = "Walid Banneni"
)

group_id <- mutate(group_id, legis_names=recode(legis_names, !!!name_recode))

# this dataset is used for the modeling part

# plot & analyze

id_plot_legis_dyn(estimate_all,
                  group_color=F,person_plot=F,text_size_label=8,use_ci = F,plot_text = F,
                highlight="Ameur Laraiedh") +
  labs(y="",x="") +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  geom_vline(aes(xintercept=lubridate::ymd('2014-10-26')),
             linetype=3) +
  geom_hline(yintercept = 0,linetype=3,color="red") +
  annotate(geom='text',x=ymd('2016-07-30'),y=22,label='Carthage\nAgreement',size=3.5) +
  annotate(geom='text',x=ymd('2014-12-2'),y=-17,label='New\nParliament',size=3.5) +
  annotate(geom='text',x=ymd('2013-09-2'),y=0,label='Ameur Laraiedh\n(Nahda Party)',size=3) +
  scale_y_continuous(labels=c('Pro-Islamist','0.0','Pro-Secular'),
                     breaks=c(-12,0.0,12)) +
  scale_color_discrete(guide='none') + 
  scale_x_date(date_breaks = '1 year',
               date_labels='%Y')

ggsave('party_over_time_2groups_1mo_ar.png',width=6,height=4)

# arp_ideal_data <- id_make(score_data = group_id,
#                           outcome="clean_votes",
#                           person_id="legis_names",
#                           item_id="law_unique",
#                           time_id="law_date",
#                           group_id="bloc",
#                           miss_val="4")

# estimate_all_rw <- id_estimate(arp_ideal_data,use_vb = T,
#                             use_groups = T,
#                             restrict_ind_high="Islamists",
#                             restrict_ind_low = "Secularists",
#                             model_type=4,
#                             vary_ideal_pts = 'spline',
#                             time_sd=.2,
#                             fixtype='vb_partial',
#                             tol_rel_obj=0.0001)

# saveRDS(estimate_all_rw,'data/estimate_all_2groups_rw_vb.rds')

# id_plot_legis_dyn(estimate_all_rw,
#                   group_color=F,person_plot=F,text_size_label=8) +
#   geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
#              linetype=2) +
#   geom_vline(aes(xintercept=lubridate::ymd('2014-10-26')),
#              linetype=3) +
#   annotate(geom='text',x=ymd('2016-07-30'),y=0.9,label=' Carthage Agreement') +
#   annotate(geom='text',x=ymd('2014-12-2'),y=0.65,label='New Parliament\nSession') +
#   scale_y_continuous(labels=c('More\nSecular','0.0','0.5','More\nIslamist'),
#                      breaks=c(-0.5,0.0,0.5,1.0)) +
#   scale_color_discrete(guide='none') + 
#   scale_x_date(date_breaks = '1 year',
#                date_labels='%Y')

# ggsave('party_over_time_2groups_1mo_rw.png')

# pull out bill discrimination parameters 

  all_params <- summary(estimate_all,pars="items")
  just_discrim <- filter(all_params,grepl(pattern = 'Non-Inflated Discrimination',x=`Item Type`)) %>% 
    mutate(abs_score=abs(`Posterior Median`),
          `item_id`=`Item Name`) %>% 
    arrange(desc(abs_score))

  group_ids <- select(estimate_all@score_data@score_matrix,item_id) %>% 
    distinct

  just_discrim <- left_join(just_discrim,group_ids,'item_id')

  all_out <- just_discrim |> 
            mutate(`5-95 Interval`=paste0("(",`Low Posterior Interval`,", ",
                      `High Posterior Interval`, ")")) |> 
                select(Vote='item_id',
                          `Discrimination Score`="Posterior Median",`5-95 Interval`) |> 
    arrange(desc(`Discrimination Score`)) |> 
              xtable()

  print(all_out,type='latex',file='discrim_bill.tex')

  # top 15 most polarizing votes (highest |discrimination|), ANC and ARP
  # separately, with the bill/vote description included. ANC item_id is
  # already the bill description (law_unique came from ANC_bill_names in
  # sessc); ARP item_id is an opaque vote_id hash, so its description is
  # built from law_title + vote_title -- law_title alone is often shared by
  # several distinct votes on the same bill (e.g. separate article votes),
  # so vote_title (e.g. "Article unique") is appended to disambiguate, and
  # falls back to vote_title alone when there is no associated law
  # (law_title == "False")

  library(tinytable)

  fix_encoding <- function(x) ifelse(validUTF8(x), x, iconv(x, from="latin1", to="UTF-8"))

  anc_desc <- distinct(sessc, law_unique) %>%
    transmute(item_id=law_unique, session="ANC",
              description=fix_encoding(law_unique))

  arp_desc <- distinct(all_votes, law_unique, vote_title, law_title) %>%
    mutate(vote_title=str_trunc(str_squish(fix_encoding(vote_title)), 50),
           law_title=str_trunc(str_squish(fix_encoding(law_title)), 90)) %>%
    transmute(item_id=law_unique, session="ARP",
              description=if_else(!is.na(law_title) & law_title != "False",
                                   paste0(law_title, " — ", vote_title),
                                   vote_title))

  item_desc <- bind_rows(anc_desc, arp_desc) %>%
    distinct(item_id, .keep_all=TRUE)

  just_discrim_desc <- left_join(just_discrim, item_desc, by="item_id")

  top15_discrim_table <- function(session_label, table_caption, table_note, file_name) {
    just_discrim_desc %>%
      filter(session == session_label) %>%
      arrange(desc(abs_score)) %>%
      slice_head(n=15) %>%
      mutate(`5-95 Interval`=paste0("(", `Low Posterior Interval`, ", ",
                                     `High Posterior Interval`, ")"),
             description=str_trunc(str_squish(description), 150)) %>%
      select(Vote=description, `Discrimination Score`=`Posterior Median`,
             `5-95 Interval`) %>%
      tt(caption=table_caption, digits=2, notes=c(table_note)) %>%
      save_tt(file_name, overwrite=TRUE)
  }

  top15_discrim_table(
    "ANC",
    "Top 15 Most Polarizing Votes, ANC (2012-2014)",
    "Table shows the 15 ANC (Assemblée Nationale Constituante) votes with the highest absolute item discrimination scores, i.e. those that best separate legislators along the ideal point scale. Estimates are posterior medians with 5-95\\% credible intervals.",
    "anc_top15_discrim.tex"
  )

  top15_discrim_table(
    "ARP",
    "Top 15 Most Polarizing Votes, ARP (2014-2019)",
    "Table shows the 15 ARP (Assemblée des Représentants du Peuple) votes with the highest absolute item discrimination scores, i.e. those that best separate legislators along the ideal point scale. Estimates are posterior medians with 5-95\\% credible intervals.",
    "arp_top15_discrim.tex"
  )

  # English versions of the two tables above, using the ~10-word English vote
  # summaries looked up from Marsad Majles (Al Bawsala) bill/vote pages -- see
  # data/marsad_vote_descriptions.csv (built separately by researching each
  # of the current top-15 votes on majles.marsad.tn). Keyed by item_id for
  # both sessions (for ANC, item_id is the French title text itself); if the
  # model is re-fit and the top 15 changes, the stopifnot() below will fail
  # loudly rather than silently show a summary for the wrong vote

  marsad_summaries <- readr::read_csv("data/marsad_vote_descriptions.csv", show_col_types=FALSE)
  summary_lookup <- setNames(marsad_summaries$summary_10_words, marsad_summaries$item_id)

  top15_discrim_table_en <- function(session_label, table_caption, table_note, file_name) {
    top15 <- just_discrim_desc %>%
      filter(session == session_label) %>%
      arrange(desc(abs_score)) %>%
      slice_head(n=15)

    stopifnot(all(top15$item_id %in% names(summary_lookup)))

    top15 %>%
      mutate(pos_med=paste0(round(`Posterior Median`, 2),", (", round(`Low Posterior Interval`,2), ", ",
                                     round(`High Posterior Interval`,2), ")"),
             description=unname(summary_lookup[item_id]),
            Polarity=case_when(`Posterior Median`>0 & session_label=="ANC"~"Islamist",
                    `Posterior Median`<0 & session_label=="ANC"~"Secularist",
                      `Posterior Median`>0 & session_label=="ARP"~"Pro-Government",
                    `Posterior Median`<0 & session_label=="ARP"~"Pro-Opposition")) %>%
      select(Vote=description, `Discrimination Score`="pos_med",
             Polarity) %>%
      tt(caption=paste0(table_caption, "\\label{tab:",session_label,"_votes}"), digits=2, notes=c(table_note)) %>%
      theme_latex(resize_width = 0.9, resize_direction = "down") |> 
      save_tt(file_name, overwrite=TRUE)
  }

  top15_discrim_table_en(
    "ANC",
    "Top 15 Most Polarizing Votes, ANC (2012-2014)",
    "Table shows the 15 ANC (Assemblée Nationale Constituante) votes with the highest absolute item discrimination scores, i.e. those that best separate legislators along the ideal point scale. Positive values indicate bills that the Islamist party Nahda supported. Estimates are posterior medians with 5-95\\% credible intervals. Vote descriptions are brief English summaries based on Marsad Majles (Al Bawsala) documentation.",
    "anc_top15_discrim_en.tex"
  )

  top15_discrim_table_en(
    "ARP",
    "Top 15 Most Polarizing Votes, ARP (2014-2019)",
    "Table shows the 15 ARP (Assemblée des Représentants du Peuple) votes with the highest absolute item discrimination scores, i.e. those that best separate legislators along the ideal point scale. Positive values indicate bills that the national unity government supported. Estimates are posterior medians with 5-95\\% credible intervals. Vote descriptions are brief English summaries based on Marsad Majles (Al Bawsala) documentation.",
    "arp_top15_discrim_en.tex"
  )

  # covariates

  id_plot_cov(estimate_all)

  library(posterior)

  cov_data <- estimate_all@stan_samples$draws("legis_x")

  # legis_x[i] corresponds position-for-position to the person-level covariate
  # columns of the score matrix, i.e. estimate_all@score_data@person_cov, which
  # is itself a subset (in the same order) of colnames(estimate_all@score_data@score_matrix)

  cov_names <- estimate_all@score_data@person_cov

  stopifnot(length(cov_names) == posterior::nvariables(cov_data))

  cov_names <- fix_encoding(cov_names)
  dimnames(cov_data)$variable <- cov_names

  # some blocs only ever served in the pre-2014 (ANC) or post-2014 (ARP) session,
  # never both -- for those, the Post-2014 Change interaction has no within-bloc
  # variation in `change` to identify it, so drop interaction terms for blocs
  # that never appear in the post-change period

  score_matrix <- estimate_all@score_data@score_matrix
  colnames(score_matrix) <- fix_encoding(colnames(score_matrix))

  bloc_cols <- grep("^person_bloc", colnames(score_matrix), value=TRUE)
  post_change_blocs <- bloc_cols[sapply(bloc_cols, function(bc) {
    any(score_matrix$person_change[score_matrix[[bc]] == 1] == 1)
  })] %>%
    str_remove("^person_bloc")

  is_interaction <- str_detect(cov_names, "^person_change:bloc")
  interaction_bloc <- str_remove(cov_names, "^person_change:bloc")

  keep <- !is_interaction | interaction_bloc %in% post_change_blocs

  cov_data <- posterior::subset_draws(cov_data, variable=cov_names[keep])
  cov_names <- cov_names[keep]

  cov_names_clean <- cov_names %>%
    str_replace_all("^person_", "") %>%
    str_replace_all("bloc", "Bloc: ") %>%
    str_replace_all("change", "Post-2014 Change")

  dimnames(cov_data)$variable <- cov_names_clean

  cov_plot <- mcmc_intervals(cov_data) +
    labs(x="Coefficient Estimate",y="",
        title="Covariate Effects on Ideal Points")

  cov_plot

  ggsave('cov_plot.png',plot=cov_plot,width=7,height=8)

  # covariate table (equivalent to arp_cov.tex) for the combined ANC-ARP model
  # build labels from the raw (post-"keep"-filter) cov_names rather than
  # cov_names_clean -- the latter's blanket "bloc" -> "Bloc: " substitution also
  # matches the literal "bloc" inside "Aucun bloc" (Independent), mangling it

  library(tinytable)

  is_interaction_kept <- str_detect(cov_names, "^person_change:bloc")
  bloc_name_kept <- str_remove(cov_names, "^person_change:bloc") %>%
    recode("Aucun bloc" = "Independent")

  anc_arp_cov_table <- summarize_draws(cov_data) %>%
    mutate(cov_names = if_else(is_interaction_kept,
                                paste("Post-2014 Change X", bloc_name_kept),
                                NA_character_)) %>%
    filter(!is.na(cov_names))

  anc_arp_cov_table %>%
    select(`Covariate`="cov_names",`5% Low`="q5",`Point Estimate`="mean",`95% High`="q95") |>
    tt(caption="Effects of Transition from ANC to ARP on Party-level Ideal Points\\label{tab:anc_effect}",digits=2,
        notes=c("Table shows estimates of party-level changes in ideal points following the transition from the ANC (National Constituent Assembly) to the ARP (Assembly of the Representatives of the People). Positive scores indicate movement in a pro-Islamist (i.e, pro-Nahda) direction. Estimates represent empirical means and quantiles of the joint posterior distribution.")) |>
    save_tt("anc_arp_cov.tex",overwrite=T)

  ## ARP-only Model

  model_type <- 1

    all_votes <- readRDS("data/all_votes.rds") %>% 
      mutate(vote_choice=na_if(vote_choice, "ABSTAIN"),
            mp_bloc_name=fct_relevel(factor(mp_bloc_name),"Nahda"))
    
    check_bills1 <- group_by(all_votes,
                            vote_id,mp_bloc_name,vote_choice,change,vote_date) %>% count %>% 
      filter(!is.na(vote_choice)) %>% 
      group_by(change,vote_id,vote_date) %>% 
      summarize(diff=mean(sqrt((sum(n[mp_bloc_name=="Front Populaire" & vote_choice=="NO"]) + sum(n[mp_bloc_name=="Nahda" & vote_choice=="YES"]))^2),na.rm=T),
                R_vote=sum(n[mp_bloc_name=="Nahda" & vote_choice=="YES"]),
                D_vote=sum(n[mp_bloc_name=="Front Populaire" & vote_choice=="YES"]),
                polarity = sign(R_vote - D_vote)) %>% 
      ungroup %>% 
      group_by(change,polarity) %>% 
      filter(diff > quantile(diff, .85,na.rm=T)) %>% 
      arrange(vote_date)
    
    print(nrow(all_votes))
    
    all_votes_small <- filter(all_votes, 
                              vote_date < ymd("2017-01-01"),
                              vote_date > ymd("2016-01-01"))

# now we need a marginal effects version of the covariate

c1 <- id_me(estimate_all, covariate="change",draws=100)

# aggregate ideal point marginal effects up to the bloc level. c1$ideal_effects
# has one row per posterior draw x item x person (marginal effect of "change"
# on that item's predicted vote probability, in the scale of the outcome).
# A person's bloc can differ across their own votes (143/428 legislators
# switched bloc between the ANC and ARP sessions -- see person_bloc* dummies
# above), so bloc is derived per score_matrix row (item x person) from those
# one-hot dummies rather than per person, then joined onto ideal_effects by
# the same (item_id, person_id) key id_me uses internally. Rows where every
# dummy is 0 (~2% of votes) are the implicit reference bloc (Afek Tounes,
# which has no dummy column since it's the omitted baseline) and are dropped
# since we can't recover the label from the data alone

bloc_mat <- as.matrix(select(score_matrix, all_of(bloc_cols)))
row_sum <- rowSums(bloc_mat)
bloc_label <- rep(NA_character_, nrow(score_matrix))
bloc_label[row_sum==1] <- str_remove(bloc_cols[max.col(bloc_mat[row_sum==1, , drop=FALSE], ties.method="first")],
                                      "^person_bloc")

bloc_lookup <- tibble(person_id=as.numeric(score_matrix$person_id),
                       item_id=as.numeric(score_matrix$item_id),
                       bloc=bloc_label) %>%
  distinct(person_id, item_id, .keep_all=TRUE) %>%
  filter(!is.na(bloc),bloc %in% c("Aucun bloc","Mouvement Nahdha","Nahda")) %>%
  group_by(person_id) |> 
  mutate(both_sess=all(c("Mouvement Nahdha","Nahda") %in% bloc),
          both_sess=replace_when(both_sess, bloc=="Aucun bloc"~NA_integer_)) |> 
  ungroup() |> 
  filter(is.na(both_sess) | both_sess) |> 
  mutate(bloc=recode(bloc, "Aucun bloc"="Independent","Mouvement Nahdha"="Nahda"))

bloc_me <- c1$ideal_effects %>%
  inner_join(bloc_lookup, by=c("person_id","item_id")) %>%
  group_by(draws, item_id, item_orig, bloc) %>%
  summarize(mean_est1=mean(estimate, na.rm=TRUE), .groups="drop_last") %>%
  group_by(bloc, item_id,item_orig) %>%
  summarize(mean_est=mean(mean_est1),
            low_est=quantile(mean_est1, .05),
            high_est=quantile(mean_est1, .95),
            .groups="drop")

# add in bill discrimination

item_discrim <- filter(estimate_all@summary,
                       grepl(x=variable, pattern="sigma\\_reg\\_free")) %>% 
  mutate(item_id=as.numeric(str_extract(variable, "[0-9]+")))

bloc_me <- left_join(bloc_me, select(item_discrim, abs_score="median", item_id),by="item_id" )

bloc_me_plot <- bloc_me %>%

  ggplot(aes(y=mean_est,
             x=reorder(item_id,mean_est))) +
  geom_linerange(aes(ymin=low_est,
                     ymax=high_est,
                     colour=abs_score)) +
  facet_wrap(~bloc) +
  ggthemes::theme_tufte() + 
  scale_colour_viridis_c(name="Vote\nPolarization",labels=c("Pro\nOpposition","0","Pro\nGovernment"),breaks=c(-0.8, 0, 0.8)) +
  coord_flip() +
    labs(x="", y="Marginal Change in Probability of Voting",
       caption=stringr::str_wrap("Plot shows, for each parliamentary bloc, the average marginal effect on voting for a specific rollcall given the change from the ANC (pre-2014) to the ARP (post-2014) sessions. Estimates are shown only for independents and members of the Nahda party who were in both sessions as other parliamentary blocs did not have surviving members from the first to second session. Point estimates are posterior means with 5-95% credible intervals across posterior draws.", 90)) +
  coord_flip() +
  geom_hline(yintercept=0,linetype=2,colour="black") +
  theme_tufte() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),legend.position = "top",legend.title.position = "top")

bloc_me_plot

ggsave("combined_me.pdf",width=6,height=8)
