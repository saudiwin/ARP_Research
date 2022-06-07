require(tidyverse)

all_votes <- read_csv("data/parliament_observatory_al_bawsala/Votes_M01.csv")

all_votes <-  mutate(all_votes,mp_bloc_name=recode(mp_bloc_name,
                                                   `Afek Tounes et l'appel des tunisiens à l'étranger`="Afek Tounes",
                                                   `Bloc Social-Démocrate`="Social-Démocrate",
                                                   `Bloc Al Horra du Mouvement Machrouu Tounes`="Horra",
                                                   `Mouvement Ennahdha`="Nahda",
                                                   `Mouvement Nidaa Tounes`='Nidaa Tounes',
                                                   `Coalition nationale`="Tahya Tounes"),
                     vote_title=coalesce(vote_title,as.character(vote_date)),
                     vote_choice=factor(vote_choice,levels=c("NO","ABSTAIN","YES")),
                     change=as.numeric(vote_date>ymd("2016-07-13")),
                     ordered_id=3,
                     change_islamist=as.numeric(mp_bloc_name=="Nahda" & change==1))

unam_votes <- group_by(all_votes, vote_id) %>% 
  summarize(unan=all(vote_choice[!is.na(vote_choice)]=="YES") || all(vote_choice[!is.na(vote_choice)]=="NO"))

all_votes <- anti_join(all_votes,filter(unam_votes,unan),by="vote_id")

# to run this model, need to remove those parties over which the covariate is not defined
all_votes <- filter(all_votes,mp_bloc_name %in% c('Nahda',
                                                  'Nidaa Tounes',
                                                  'Front Populaire',
                                                  "Social-Démocrate",
                                                  "Afek Tounes",
                                                  "Tahya Tounes",
                                                  "Horra",
                                                  "Union Patriotique Libre",
                                                  "Bloc Démocrate",
                                                  "Aucun bloc")) %>% 
  mutate(mp_bloc_name=recode(mp_bloc_name,
                             `Bloc Démocrate`="Social-Démocrate"))

bill_stat <- all_votes %>% 
  group_by(vote_id) %>% 
  count(vote_choice)

bill_stat_nahda <- filter(all_votes, mp_bloc_name=="Nahda") %>% 
  group_by(vote_id) %>% 
  count(vote_choice,.drop=F)

bill_stat_fp <- filter(all_votes, mp_bloc_name %in% c("Nidaa Tounes", "Tahya Tounes","Horra")) %>% 
  group_by(vote_id) %>% 
  count(vote_choice,.drop=F)

bill_stat <- left_join(bill_stat_fp,bill_stat_nahda, by=c("vote_id","vote_choice")) %>% 
  filter(!is.na(vote_choice))

bill_stat_yes <- filter(bill_stat, vote_choice=="YES") %>% 
  mutate(vote_diff=sqrt((n.x - n.y)^2))
bill_stat_no <- filter(bill_stat, vote_choice=="NO") %>% 
  mutate(vote_diff=sqrt((n.x - n.y)^2))

spec_leg <- group_by(all_votes, mp_id) %>% 
  summarize(n_vote=length(unique(vote_date)))

all_votes <- anti_join(all_votes, filter(spec_leg, n_vote<30))

saveRDS(all_votes, "data/all_votes.rds")
