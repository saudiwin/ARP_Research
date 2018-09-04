# let's see how this thing does running all of our data!


require(bayesplot)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(forcats)


clean_data <- readr::read_csv('data/clean_votes_groups.csv')

select(clean_data,law_unique,law_date) %>% 
  distinct %>% 
  ggplot(aes(x=law_date)) + geom_histogram(fill='grey',
                                                   colour=NA) + 
  theme_minimal() + xlab('') + ylab('Number of Roll Call Votes') +
  theme(panel.grid=element_blank()) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  annotate(geom='text',x=ymd('2016-07-30'),y=450,label='Carthage Agreement') +
  ggtitle('Legislative Activity in the Tunisian Parliament')
ggsave('bill_density.png')

# get blocs 
source('R_Scripts/import_legis.R')
just_blocs <- readRDS('data/just_blocs.rds') %>% 
  ungroup %>% 
  mutate(her_name=textclean::replace_non_ascii(her_name))

# recode chettaoui

just_blocs$her_name[grepl(pattern = 'Chettaoui',x=just_blocs$her_name)] <- "Leila Chettaoui Bougatef"

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