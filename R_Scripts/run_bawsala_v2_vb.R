# let's see how this thing does running all of our data!

require(idealstan)
require(bayesplot)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(forcats)
require(xtable)


group_id <- readr::read_csv('data/clean_votes_groups.csv')

group_id$bloc <-  recode(group_id$bloc,
                                                         `Afek Tounes et l'appel des tunisiens l'tranger`="Afek Tounes",
                                                         `Bloc Social-Dmocrate`="Social-Démocrate",
                                                         `Bloc Al Horra du Mouvement Machrouu Tounes`="Horra",
                                                         `Mouvement Ennahdha`="Nahda",
                                                         `Mouvement Nidaa Tounes`='Nidaa Tounes',
                         `Alliance Dmocratique`="Alliance Démocratique",
                         `AllA(C)geance A la Patrie`="Allégeance A la Patrie")

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
group_id <- group_id %>% complete(law_date,nesting(bloc,change),fill=list(change=0))

# first run an AR(1) model with covariates

arp_ideal_data <- id_make(score_data = group_id,
                          outcome="clean_votes",
                          person_id="legis_names",
                          item_id="law_unique",
                          time_id="law_date",
                          group_id="bloc",
                          group_cov=~change*bloc,
                          miss_val="4")



estimate_all <- id_estimate(arp_ideal_data,use_vb = T,
                            use_groups = T,
                            restrict_ind_high= "Nahda",
                            restrict_ind_low="Front Populaire",
                            model_type=4,
                            restrict_var = T,
                            restrict_var_high = 0.25,
                            vary_ideal_pts = 'AR1',
                            time_sd=1,
                            fixtype='vb_partial',
                            tol_rel_obj=0.0001)



saveRDS(estimate_all,'data/estimate_all_ar1_vb.rds')



arp_ideal_data <- id_make(score_data = group_id,
                          outcome="clean_votes",
                          person_id="legis_names",
                          item_id="law_unique",
                          time_id="law_date",
                          group_id="bloc",
                          miss_val="4")

estimate_all_rw <- id_estimate(arp_ideal_data,use_vb = T,
                            use_groups = T,
                            restrict_ind_high= "Nahda",
                            restrict_ind_low="Front Populaire",
                            model_type=4,
                            restrict_var = T,
                            restrict_var_high = 0.1,
                            vary_ideal_pts = 'random_walk',
                            time_sd=1,
                            fixtype='vb_partial',niters=1000,
                            tol_rel_obj=0.0001)



saveRDS(estimate_all_rw,'data/estimate_all_rw_vb.rds')




