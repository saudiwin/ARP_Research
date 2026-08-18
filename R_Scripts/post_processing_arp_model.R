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

# covariates

id_plot_cov(estimate_all)

library(posterior)

cov_data <- estimate_all@stan_samples$draws("legis_x")

# legis_x[i] corresponds position-for-position to the person-level covariate
# columns of the score matrix, i.e. estimate_all@score_data@person_cov, which
# is itself a subset (in the same order) of colnames(estimate_all@score_data@score_matrix)

cov_names <- estimate_all@score_data@person_cov

stopifnot(length(cov_names) == posterior::nvariables(cov_data))

fix_encoding <- function(x) ifelse(validUTF8(x), x, iconv(x, from="latin1", to="UTF-8"))

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
