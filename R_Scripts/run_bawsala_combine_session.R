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
require(textclean)

# need to load and edit first session

sess1 <- readxl::read_excel('bawsala/inst/extdata/ANC_votes.xlsx') %>% 
  gather(key = id,value=outcome,-legis.names,-bloc) %>% 
  mutate(id=as.numeric(id),
         outcome=ifelse(is.na(outcome),'absent',outcome),
         outcome=factor(outcome,levels=c('contre','abstenu','pour','absent')))

sess1_billnames <- readxl::read_excel('data/ANC_votes_labels-HM-edits.xlsx') %>% 
  select(-X__1)


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

group_id <- readr::read_csv('data/clean_votes_groups.csv')
day(group_id$law_date) <- 10

# combine sessions

group_id <- bind_rows(group_id,sessc) %>% 
  mutate(bloc=fct_collapse(bloc,
                           Islamists=c("Mouvement Nahdha",
                                       "Mouvement Ennahdha"),
                           Secularists=c("Afek Tounes et l'appel des tunisiens l'tranger",
                                         "AllA(C)geance A la Patrie",
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
                                         "Transition D\xe9mocratique")))


arp_ideal_data <- id_make(score_data = group_id,
                          outcome="clean_votes",
                          person_id="legis_names",
                          item_id="law_unique",
                          time_id="law_date",
                          group_id="bloc",
                          person_data=distinct(select(group_id,person.names=legis_names,
                                                      group=bloc,
                                                      time=law_date)),
                          miss_val=4L)

estimate_all <- id_estimate(arp_ideal_data,use_vb = T,
                            use_groups = T,
                            restrict_ind_high=1,
                            model_type=4,
                            vary_ideal_pts = 'AR1',
                            time_sd=.2,
                            fixtype='vb_partial',
                            tol_rel_obj=0.001)

saveRDS(estimate_all,'data/estimate_all_2groups_vb.rds')

id_plot_legis_dyn(estimate_all,
                  group_color=F,person_plot=F,text_size_label=8) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  geom_vline(aes(xintercept=lubridate::ymd('2014-10-26')),
             linetype=3) +
  annotate(geom='text',x=ymd('2016-07-30'),y=0.9,label=' Carthage Agreement') +
  annotate(geom='text',x=ymd('2014-10-26'),y=0.65,label='New Parliament\nSession') +
  scale_y_continuous(labels=c('More\nSecular','0.0','0.5','More\nIslamist'),
                     breaks=c(-0.5,0.0,0.5,1.0)) +
  scale_color_discrete(guide='none') + 
  scale_x_date(date_breaks = '1 year',
               date_labels='%Y')

ggsave('party_over_time_2groups_1mo.png')

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

