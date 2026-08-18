# let's see how this thing does running all of our data!

library(cmdstanr)

set_cmdstan_path("/scratch/user/u.rk234736/cmdstan-2.39.0")

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

#group_id$clean_votes <- factor(group_id$clean_votes)

arp_ideal_data <- id_make(score_data = group_id,
                          outcome_disc="clean_votes",
                          person_id="legis_names",
                          item_id="law_unique",
                          time_id="law_date",
                          person_cov = ~change*bloc)

estimate_all <- id_estimate(arp_ideal_data,
                            restrict_ind_high=c("554ced8712bdaa5df2537688","569416d212bdaa5ee3796068",
                              "59b287314f24d0311313bfff","5ba234a14f24d03ba3d842a2"),
                              restrict_ind_low=c("5603199812bdaa20aa5b4907",
                                                 "5c9cf4aa4f24d0572feb077c","57a31e71cf44122088ceed2c",
                                                 "5866afa7cf44121f3e63b001"),
                              #restrict_ind_high="57a31e71cf44122088ceed2c",
                              #restrict_ind_low="57a31e71cf44122088ceed31",
                              const_type="items",
                            model_type=2,
                            vary_ideal_pts = 'splines',
                            spline_degree=2,spline_knots = c(min(unique(arp_ideal_data@score_matrix$time_id)),
                                                              lubridate::mdy('12-2-2014'),
                                                            max(unique(arp_ideal_data@score_matrix$time_id))),
                              nchains = 4,max_treedepth=13,
                              ncores = parallel::detectCores(),
                              fixtype='prefix',niters = 500,
                              warmup=500,id_refresh=10)

saveRDS(estimate_all,'data/estimate_all_2groups_ar_vb.rds')


