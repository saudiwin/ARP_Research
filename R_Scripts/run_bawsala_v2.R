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


# create vote matrix

to_ideal_wide <- select(group_id,law_unique,clean_votes,legis_names) %>% 
  spread(key = law_unique,value = clean_votes)

# readr::write_csv(select(to_ideal,-same_day),'data/clean_bawsala_votes.csv')
# 
# check_ideal <- group_by(to_ideal,law_unique,legis_names) %>% 
#   count %>% filter(n>1)

vote_matrix <- dplyr::select(ungroup(to_ideal_wide),-legis_names) %>% as.matrix
row.names(vote_matrix) <- to_ideal_wide$legis_names
#colnames(vote_matrix) <- paste0('Bill_',1:ncol(vote_matrix))
get_time <- distinct(select(ungroup(group_id),law_unique,law_date))
arp_ideal_data <- id_make(score_data = group_id,
                          outcome="clean_votes",
                          person_id="legis_names",
                          item_id="law_unique",
                          time_id="law_date",
                          group_id="bloc",
                          person_cov=~change,
                          inflate=T,
                          person_data=distinct(select(group_id,person.names=legis_names,
                                                      group=bloc,
                                                      time=law_date)),
                          miss_val=4L)

estimate_all <- id_estimate(arp_ideal_data,use_vb = T,
                            use_groups = T,nfix = 1,
                            restrict_ind_high = 9,
                            restrict_ind_low=7,
                            model_type=4,
                            use_ar=T,
                            id_diff=2,
                            time_sd=20,
                            fixtype='constrained')

estimate_all@score_data@score_matrix$group_id <-  recode(estimate_all@score_data@score_matrix$group_id,
                                                                        `Afek Tounes et l'appel des tunisiens l'tranger`="Afek Tounes",
                                                                        `Bloc Social-Dmocrate`="Social-Démocrate",
                                                                        `Bloc Al Horra du Mouvement Machrouu Tounes`="Horra",
                                                                        `Mouvement Ennahdha`="Nahda",
                                                         `Mouvement Nidaa Tounes`='Nidaa Tounes')

saveRDS(estimate_all,'data/estimate_all_vb.rds')

id_plot_legis_dyn(estimate_all,highlight = c('Horra','Front Populaire',
                                             'Nahda','Nidaa Tounes'),
                  group_color=F,person_plot=F) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  scale_y_continuous(labels=c('More\nSecular','-1.0','-0.5','0.0','0.5','More\nIslamist'),
                     breaks=c(-1.5,-1.0,-0.5,0.0,0.5,1.0)) +
  facet_wrap(~group_id)

ggsave('party_over_time.png')

# pull out bill discrimination parameters 

all_params <- summary(estimate_all)
just_discrim <- filter(all_params,grepl(pattern = 'sigma_reg_free',x=parameters)) %>% 
  mutate(abs_score=abs(posterior_median),
         index=as.numeric(str_extract(parameters,'[0-9]+'))) %>% 
  arrange(desc(abs_score))
group_ids <- select(estimate_all@score_data@score_matrix,item_id) %>% 
  mutate(index=as.numeric(item_id)) %>% 
  distinct

just_discrim <- left_join(just_discrim,group_ids,'index')

all_out <- xtable(select(just_discrim,
                         Vote='item_id',
                         `Discrimination Score`="posterior_median",
                        `Standard Deviation (Error)`="posterior_sd"))
print(all_out,type='latex',file='discrim_bill.tex')

