# let's see how this thing does running all of our data!

require(idealstan)
require(bayesplot)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(forcats)


all_baws_data <- readRDS('data/all_bawala2018.rds')

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

# get blocs 
source('R_Scripts/import_legis.R')
just_blocs <- readRDS('data/just_blocs.rds') %>% 
  ungroup %>% 
  mutate(her_name=textclean::replace_non_ascii(her_name))

# recode chettaoui

just_blocs$her_name[grepl(pattern = 'Chettaoui',x=just_blocs$her_name)] <- "Leila Chettaoui Bougatef"



#make a matrix for idealstan

to_ideal <- select(recode_baws,legis_names,clean_votes,law_title,law_type,law_date) %>% 
  mutate(law_unique=paste(law_title,law_type,law_date)) %>% 
  select(legis_names,clean_votes,law_unique,law_date) %>% 
  mutate(clean_votes=factor(clean_votes,levels=c('contre','abstenu','pour','absent',
                                                 'excuse')),
         clean_votes=fct_recode(clean_votes,absent='excuse'),
         clean_votes=as.numeric(clean_votes))



# we have duplicate records when people voted twice on the same thing in the same day
# require(readr)
# 
# examine <- table(to_ideal$law_unique)
# examine <- examine[examine>length(unique(to_ideal$legis_names))]
# # # to_remove <- filter(to_ideal,law_unique %in% names(examine))
# # recoded <-  c(names(examine),)
# # load list of laws that had to be changed to make unique
# 
# to_remove_changed <- read_csv('data/to_remove.csv') %>% 
#   filter(same_day!='Remove') %>% 
#   mutate(clean_votes=factor(clean_votes,levels=c('contre','abstenu','pour','absent',
#                                                  'excuse')),
#          clean_votes=fct_recode(clean_votes,absent='excuse'),
#          clean_votes=as.numeric(clean_votes)) %>% 
#   filter(!(clean_votes %in% c(3,4)))
# to_ideal <- filter(to_ideal,!(law_unique %in% c(names(examine),
#                                                 "Projet de loi N°09/2016 relatif aux banques et aux établissements financiers Article 32 2016-05-11",
#                                                 "Projet de loi N°25/2016 relatif à la révision des avantages fiscaux Principe de passage au vote sur les articles 2017-02-01")))
# to_ideal <- bind_rows(to_ideal,to_remove_changed)
# to_ideal <- mutate(to_ideal,
#                    law_unique=ifelse(is.na(same_day),
#                                      law_unique,
#                                       paste(law_unique,same_day)))
# now the key is unique even for multiple same-day votes

# there are some coding errors where people were marked absent but were instead present. need to fix those

to_ideal <- group_by(to_ideal,law_unique,legis_names) %>% 
  mutate(clean_votes=ifelse(n()>1 && 4 %in% clean_votes,min(clean_votes),clean_votes)) %>% 
  distinct %>% 
  ungroup %>% 
  mutate(legis_names=recode(legis_names,
                            `Mbarka Aouaniaξ`="Mbarka Aouania"),
         legis_names=textclean::replace_non_ascii(iconv(legis_names,from='LATIN1','UTF-8')),
         legis_names=recode(legis_names,
                            `Mbarka Aouaniaae`="Mbarka Aouania",
                            `Mbarka AouaniaI 3/4`="Mbarka Aouania",
                            `Mbarka AouaniaA`="Mbarka Aouania",
                            `Leila UChettaoui Bougatef`="Leila Chettaoui Bougatef"))

#to_ideal$legis_names[grepl(pattern = 'Chettaoui',x=to_ideal$legis_names)] <- "Leila Chettaoui Bougatef"

# iterate over legislators to get time-varying group IDs

  
group_id <- group_by(to_ideal,legis_names) %>% 
    mutate(this_legis_bloc=just_blocs$bloc_name[just_blocs$her_name==legis_names[1]],
           this_legis_bloc2=just_blocs$bloc2_name[just_blocs$her_name==legis_names[1]],
           date_change=just_blocs$bloc_end[just_blocs$her_name==legis_names[1]],
           bloc=case_when(is.na(this_legis_bloc2)~this_legis_bloc,
             law_date<date_change~this_legis_bloc,
            law_date>=date_change~this_legis_bloc2),
           change=ifelse(!is.na(this_legis_bloc2) & this_legis_bloc2=='Bloc Al Horra du Mouvement Machrouu Tounes',
                         as.numeric(law_date>date_change),0),
           bloc=textclean::replace_non_ascii(iconv(bloc,from='LATIN1','UTF-8')),
           bloc=recode(bloc,
                       `Afek Tounes et l'appel des tunisiens A l'A(C)tranger`="Afek Tounes et l'appel des tunisiens l'tranger",
                       `Bloc DA(C)mocrate`="Bloc Social-Dmocrate",
                       `Bloc Social-DA(C)mocrate`="Bloc Social-Dmocrate",
                       `FrontEPopulaire`="Front Populaire"),
           bloc_id=as.numeric(factor(bloc)))

# this is the full data, so let's save a clean version

readr::write_csv(group_id,'data/clean_votes_groups.csv')


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

estimate_all <- id_estimate(arp_ideal_data,use_vb = F,
                            use_groups = T,nfix = 1,
                            restrict_ind_high = 2,
                            restrict_ind_low=1,
                            model_type=4,
                            use_ar=T,
                            id_diff=4,
                            time_sd=20,
                            fixtype='vb',
                            niters = 1000)



id_plot_legis_dyn(estimate_all,highlight = c('Bloc Al Horra du Mouvement Machrouu Tounes'),
                  group_color=F) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2)
