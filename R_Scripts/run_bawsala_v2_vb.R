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
                            tol_rel_obj=0.001)



saveRDS(estimate_all,'data/estimate_all_ar1_vb.rds')

# need to reverse the scale because the ID strategy ends up reversing itself
# for compatibility with the RW model

id_plot_legis_dyn(estimate_all,person_plot=F,use_ci = F) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  scale_y_continuous(labels=c('More\nIslamist','1','0.5','0.0','-0.5','-1.0','-1.5','More\nSecular'),
                     breaks=c(-2.5,-1.75,-1,0.0,1,1.75,2.5,3.5)) +
  guides(colour='none') +
  facet_wrap(~group_id,scales='free_y')

ggsave('party_over_time_ar1_vb.png')

id_plot_cov(estimate_all) + scale_x_reverse(labels=c('More\nIslamist',
                                                     '5',
                                                     '0',
                                                     '-5',
                                                     'More \nSecular')) +
  ggtitle('Effect of Carthage Agreement on Party-level Ideal Points',
          subtitle = 'Based on Rollcall Vote Data from the Tunisian National Representative Assembly (ARP)')

ggsave('id_plot_cov_arp_all.png')

id_plot_cov(estimate_all,filter_cov = c('change:blocHorra','change')) + scale_y_reverse()

ggsave('id_plot_cov_arp_horra.png')

id_plot_legis_var(estimate_all,person_labels = F)

ggsave('id_plot_var_arp_ar1.png')

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
                            tol_rel_obj=0.001)



saveRDS(estimate_all_rw,'data/estimate_all_rw_vb.rds')


id_plot_legis_dyn(estimate_all_rw,person_plot=F,use_ci = F,
                  highlight=c('Nahda','Nidaa Tounes','Front Populaire','Horra')) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  scale_y_continuous(labels=c('More\nSecular','-5.0','-2.5','-1.0','0.0','More\nIslamist'),
                     breaks=c(-10,-5,-2.5,-1,0.0,1.5)) +
  guides(colour='none')

ggsave('party_over_time_rw_vb.png')

id_plot_legis_dyn(estimate_all_rw,person_plot=F,use_ci = T) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  scale_y_continuous(labels=c('More\nSecular','-5.0','-2.5','-1.0','0.0','More\nIslamist'),
                     breaks=c(-10,-5,-2.5,-1,0.0,1.5)) +
  guides(colour='none') +
  facet_wrap(~group_id,scales='free_y')

ggsave('party_over_time_panel_rw_vb.png')

id_plot_legis_var(estimate_all_rw,person_labels = F)

ggsave('id_plot_var_arp_rw.png')

# pull out bill discrimination parameters 

all_params <- summary(estimate_all_rw,pars='items')
just_discrim <- filter(all_params,grepl(pattern = 'sigma_reg_free',x=parameters)) %>% 
  mutate(abs_score=abs(posterior_median),
         index=as.numeric(str_extract(parameters,'[0-9]+'))) %>% 
  arrange(desc(abs_score))
group_ids <- select(estimate_all_rw@score_data@score_matrix,item_id) %>% 
  mutate(index=as.numeric(item_id)) %>% 
  distinct

just_discrim <- left_join(just_discrim,group_ids,'index')

all_out <- xtable(select(just_discrim,
                         Vote='item_id',
                         `Discrimination Score`="posterior_median",
                        `Standard Deviation (Error)`="posterior_sd"))
print(all_out,type='latex',file='discrim_bill.tex')

