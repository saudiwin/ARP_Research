# let's see how this thing does running all of our data!

require(idealstan)
require(bayesplot)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(forcats)


all_baws_data <- readRDS('data/all_bawala.rds')

recode_baws <- mutate(all_baws_data,law_date=if_else(law_title==law_date,
                                       law_type,
                                       law_date),
                      law_type=if_else(law_type==law_date,
                                       "Projet de loi en sa totalité",
                                       law_type),
  law_day=str_extract(law_date,'[0-9]{1,2}'),
                      law_month=str_extract(law_date,'[\\p{L}\\p{M}]+'),
                      law_year=str_extract(law_date,'[0-9]{4}'),
  law_month=recode(law_month,
                   `août`='August',
                   `avril`='April',
                   `décembre`='December',
                   `février`='February',
                   `janvier`='January',
                   `juillet`='July',
                   `juin`='June',
                   `mai`='May',
                   `mars`='March',
                   `novembre`='November',
                   `octobre`='October',
                   `septembre`='September'),
  law_date=dmy(paste(law_day,law_month,law_year)),
  law_duration=as.duration(interval(min(law_date),law_date))) 

ggplot(recode_baws,aes(x=law_date)) + geom_density(fill='grey',
                                                   colour=NA) + 
  theme_minimal() + xlab('') + ylab('Density of Bills') +
  theme(panel.grid=element_blank())
ggsave('bill_density.png')

#make a matrix for idealstan

to_ideal <- select(recode_baws,legis_names,clean_votes,law_title,law_type,law_date) %>% 
  mutate(law_unique=paste(law_title,law_type,law_date)) %>% 
  select(legis_names,clean_votes,law_unique) %>% 
  mutate(clean_votes=factor(clean_votes,levels=c('contre','abstenu','pour','absent',
                                                 'excuse')),
         clean_votes=fct_recode(clean_votes,absent='excuse'),
         clean_votes=as.numeric(clean_votes))

# we have duplicate records when people voted twice on the same thing in the same day
require(readr)

examine <- table(to_ideal$law_unique)
examine <- examine[examine>length(unique(to_ideal$legis_names))]
# # to_remove <- filter(to_ideal,law_unique %in% names(examine))
# recoded <-  c(names(examine),)
# load list of laws that had to be changed to make unique

to_remove_changed <- read_csv('data/to_remove.csv') %>% 
  filter(same_day!='Remove') %>% 
  mutate(clean_votes=factor(clean_votes,levels=c('contre','abstenu','pour','absent',
                                                 'excuse')),
         clean_votes=fct_recode(clean_votes,absent='excuse'),
         clean_votes=as.numeric(clean_votes))
to_ideal <- filter(to_ideal,!(law_unique %in% c(names(examine),
                                                "Projet de loi N°09/2016 relatif aux banques et aux établissements financiers Article 32 2016-05-11",
                                                "Projet de loi N°25/2016 relatif à la révision des avantages fiscaux Principe de passage au vote sur les articles 2017-02-01")))
to_ideal <- bind_rows(to_ideal,to_remove_changed)
to_ideal <- mutate(to_ideal,
                   law_unique=ifelse(is.na(same_day),
                                     law_unique,
                                      paste(law_unique,same_day)))
# now the key is unique even for multiple same-day votes
to_ideal_wide <- select(to_ideal,-same_day) %>% 
  spread(key = law_unique,value = clean_votes)

check_ideal <- group_by(to_ideal,law_unique,legis_names) %>% 
  count %>% filter(n>1)

vote_matrix <- dplyr::select(to_ideal_wide,-legis_names) %>% as.matrix
row.names(vote_matrix) <- to_ideal_wide$legis_names
colnames(vote_matrix) <- paste0('Bill_',1:ncol(vote_matrix))
arp_ideal_data <- id_make(vote_data = vote_matrix,
                          legis_data=select(to_ideal_wide,legis_names),
                          abs_vote=4L)

estimate_arp <- id_estimate(arp_ideal_data,
                            model_type=4,
                            abs_discrim_sd = 5,
                            reg_discrim_sd = 5,
                            legis_sd = 1,
                            diff_sd=5,
                            nfix=10,
                            use_vb = T,
                            restrict_type='constrain_twoway',
                            fixtype='vb',restrict_params = 'legis',
                            seed=84520)
saveRDS(estimate_arp,file = 'estimate_arp.rds')