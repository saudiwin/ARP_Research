# Version 3: Using full ARP for session I/II
# Robert Kubinec 

.libPaths("/home/rmk7/other_R_libs3")

require(cmdstanr)

set_cmdstan_path("/home/rmk7/cmdstan")

require(idealstan)
require(bayesplot)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(forcats)
require(xtable)

# legislative data 
# provided by Al-Bawsala
all_votes <- readr::read_csv('/scratch/rmk7/arp/Votes_M01.csv')

all_votes <-  mutate(all_votes,mp_bloc_name=recode(mp_bloc_name,
                                                         `Afek Tounes et l'appel des tunisiens à l'étranger`="Afek Tounes",
                                                         `Bloc Social-Démocrate`="Social-Démocrate",
                                                         `Bloc Al Horra du Mouvement Machrouu Tounes`="Horra",
                                                         `Mouvement Ennahdha`="Nahda",
                                                         `Mouvement Nidaa Tounes`='Nidaa Tounes'),
                     vote_title=coalesce(vote_title,as.character(vote_date)),
                     bill_id=fct_anon(fct_cross(vote_title,law_title)),
                     vote_choice=factor(vote_choice,levels=c("NO","ABSTAIN","YES")),
                     change=as.numeric(vote_date>ymd("2016-07-13")),
                     ordered_id=3)

# need to do some bill analytics  / remove bills for which we have little data

bill_stat <- group_by(all_votes, bill_id) %>% 
  count(vote_choice)

# remove if yes/no < 3

bill_stat_trim <- spread(bill_stat, key = "vote_choice",value="n") %>% 
  filter(YES<3 | NO < 3)

all_votes_trim <- anti_join(all_votes,bill_stat_trim,"bill_id")

# check legislators

leg_count <- group_by(all_votes_trim, mp_id) %>% 
  summarize(votes=sum(vote_choice %in% c("YES","NO")))

leg_trim <- filter(leg_count, votes<10)
                     

all_votes_trim <- anti_join(all_votes_trim, leg_trim, by="mp_id")

# collapse days into weeks

all_votes_trim <- mutate(all_votes_trim,
                         vote_month=paste0(year(vote_date),"-",month(vote_date)),
                         vote_month=factor(vote_month))

# to run this model, need to remove those parties over which the covariate is not defined
# group_id <- filter(group_id,bloc %in% c('Nahda',
#                                         'Nidaa Tounes',
#                                         'Front Populaire',
#                                         "Social-Démocrate",
#                                         "Afek Tounes",
#                                         "Union Patriotique Libre",
#                                         "Alliance Dmocratique",
#                                         "Independent"))

# we need to complete the data for horra and others who aren't in every time point
# this is for group covariate plotting
# all_votes <- all_votes %>% complete(law_date,nesting(bloc,change),fill=list(change=0))

# first run an AR(1) model with covariates

arp_ideal_data <- id_make(score_data = all_votes_trim,
                          outcome_disc="vote_choice",
                          person_id="mp_id",
                          item_id="bill_id",
                          time_id="vote_month",
                          group_id="mp_bloc_name",
                          person_cov=~change*mp_bloc_name)



estimate_all <- id_estimate(arp_ideal_data,
                            use_groups = T,
                            restrict_ind_high= "Nahda",
                            restrict_ind_low="Front Populaire",
                            restrict_sd_low = .001,
                            restrict_sd_high=.001,
                            fix_low = 0,
                            model_type=4,save_files="/scratch/rmk7/arp/junk/",
                            vary_ideal_pts = 'random_walk',nchains = 2,
                            ncores = parallel::detectCores(),
                            fixtype='prefix',niters = 300,warmup=300,
                            id_refresh=10)



saveRDS(estimate_all,'/scratch/rmk7/arp/estimate_all_ar1_full.rds')

