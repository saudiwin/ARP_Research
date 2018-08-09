# examine and import legislative data from bawsalah
# Robert Kubinec
# July 30, 2018

require(readr)
require(dplyr)
require(stringr)
require(tidyr)

legis_data <- readRDS('all_parliament_legisdata.rds')

just_blocs <- group_by(legis_data,session) %>% 
  # filter(!is.na(bloc_name)) %>% 
  mutate(session_id=as.numeric(factor(session,levels=c('Quatrième session parlementaire ',
                                            'Troisième session extraordinaire ',
                                            'Troisième session ordinaire',
                                            'Troisième session parlementaire',
                                            'Deuxième session extraordinaire',
                                            'Deuxième session ordinaire',
                                            'Deuxième session parlementaire',
                                            'Première session parlementaire'))),
         bloc_name=case_when(her_name %in% c('Nesrine Laameri',
                                             'Maroua Bouazzi')~'Mouvement Nidaa Tounes',
                             her_name %in% c('Yassine Ayari',
                                             'Abderrazak Chraiet')~'Independent',
                             TRUE~bloc_name),
         bloc2_name=if_else(her_name=='Nesrine Laameri','Bloc Al Horra du Mouvement Machrouu Tounes',
                            bloc2_name),
         list_name=recode(list_name,`Parti l`="Parti l'initiative",
                                      `Al Jomhouri`="Congrès Pour la République")) %>% 
  group_by(her_name) %>% 
  filter(session_id==min(session_id))

# import missing legislators

missing_legis <- readr::read_csv('data/missing_legis.csv') %>% 
  mutate(bloc_start=mdy(bloc_start),
         bloc_end=mdy(bloc_end))

just_blocs <- bind_rows(just_blocs,missing_legis)

saveRDS(object = just_blocs,'data/just_blocs.rds')
readr::write_csv(just_blocs,'data/just_blocs.csv')
