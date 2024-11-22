# Version 3: Using full ARP for session I/II
# Robert Kubinec 



require(cmdstanr)
require(idealstan)
require(bayesplot)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(forcats)
require(xtable)

model_type <- 1

# legislative data 
# provided by Al-Bawsala

  # all_votes <- readr::read_csv('data/parliament_observatory_al_bawsala/Votes_M01.csv')
  # 
  # all_votes <-  mutate(all_votes,mp_bloc_name=recode(mp_bloc_name,
  #                                                    `Afek Tounes et l'appel des tunisiens à l'étranger`="Afek Tounes",
  #                                                    `Bloc Social-Démocrate`="Social-Démocrate",
  #                                                    `Bloc Al Horra du Mouvement Machrouu Tounes`="Horra",
  #                                                    `Mouvement Ennahdha`="Nahda",
  #                                                    `Mouvement Nidaa Tounes`='Nidaa Tounes',
  #                                                    `Coalition nationale`="Tahya Tounes"),
  #                      vote_title=coalesce(vote_title,as.character(vote_date)),
  #                      vote_choice=factor(vote_choice,levels=c("NO","ABSTAIN","YES")),
  #                      change=as.numeric(vote_date>ymd("2016-07-13")),
  #                      ordered_id=3)
  # 
  # unam_votes <- group_by(all_votes, vote_id) %>% 
  #   summarize(unan=all(vote_choice[!is.na(vote_choice)]=="YES") || all(vote_choice[!is.na(vote_choice)]=="NO"))
  # 
  # all_votes <- anti_join(all_votes,filter(unam_votes,unan),by="vote_id")
  # 
  # # to run this model, need to remove those parties over which the covariate is not defined
  # all_votes <- filter(all_votes,mp_bloc_name %in% c('Nahda',
  #                                                   'Nidaa Tounes',
  #                                                   'Front Populaire',
  #                                                   "Social-Démocrate",
  #                                                   "Afek Tounes",
  #                                                   "Tahya Tounes",
  #                                                   "Horra",
  #                                                   "Union Patriotique Libre",
  #                                                   "Bloc Démocrate",
  #                                                   "Aucun bloc")) %>% 
  #   mutate(mp_bloc_name=recode(mp_bloc_name,
  #                              `Bloc Démocrate`="Social-Démocrate",
  #                              `Tahya Tounes`="Nidaa Tounes"))
  # 
  # bill_stat <- all_votes %>% 
  #   group_by(vote_id) %>% 
  #   count(vote_choice)
  # 
  # bill_stat_nahda <- filter(all_votes, mp_bloc_name=="Nahda") %>% 
  #   group_by(vote_id) %>% 
  #   count(vote_choice,.drop=F)
  # 
  # bill_stat_fp <- filter(all_votes, mp_bloc_name %in% c("Nidaa Tounes", "Tahya Tounes","Horra")) %>% 
  #   group_by(vote_id) %>% 
  #   count(vote_choice,.drop=F)
  # 
  # bill_stat <- left_join(bill_stat_fp,bill_stat_nahda, by=c("vote_id","vote_choice")) %>% 
  #   filter(!is.na(vote_choice))
  # 
  # bill_stat_yes <- filter(bill_stat, vote_choice=="YES") %>% 
  #   mutate(vote_diff=sqrt((n.x - n.y)^2))
  # bill_stat_no <- filter(bill_stat, vote_choice=="NO") %>% 
  #   mutate(vote_diff=sqrt((n.x - n.y)^2))

  # use script create_data.R to create the rds file
  
  all_votes <- readRDS("data/all_votes.rds") %>% 
    mutate(vote_choice=na_if(vote_choice, "ABSTAIN"),
           mp_bloc_name=fct_relevel(factor(mp_bloc_name),"Nahda"))
  
  check_bills1 <- group_by(all_votes,
                           vote_id,mp_bloc_name,vote_choice,change,vote_date) %>% count %>% 
    filter(!is.na(vote_choice)) %>% 
    group_by(change,vote_id,vote_date) %>% 
    summarize(diff=mean(sqrt((n[mp_bloc_name=="Front Populaire" & vote_choice=="NO"] + n[mp_bloc_name=="Nahda" & vote_choice=="YES"])^2),na.rm=T),
              R_vote=n[mp_bloc_name=="Nahda" & vote_choice=="YES"],
              D_vote=n[mp_bloc_name=="Front Populaire" & vote_choice=="YES"],
              polarity = sign(R_vote - D_vote)) %>% 
    ungroup %>% 
    group_by(change,polarity) %>% 
    filter(diff > quantile(diff, .85,na.rm=T)) %>% 
    arrange(vote_date)
  
  # no vote: approving budget for the IVD
  # NT split with Nahda, voted with Horra & FP in favor of law
  # Report de l'examen du budget de l'instance vérité et dignité
  # 58528919cf44121f3e63aee5
  
  # yes vote: Nahda vote for an amendment to 2018 budget (passed in Dec 2017)
  # link: https://twitter.com/AlBawsalaTN/status/939581479952289792/photo/1
  # not entirely sure what the amendment is about
  # vote ID 5aeae5b84f24d02328a2f1aa
  
  # we need to complete the data for horra and others who aren't in every time point
  # this is for group covariate plotting
  # all_votes <- all_votes %>% complete(law_date,nesting(bloc,change),fill=list(change=0))
  
  # first run an AR(1) model with covariates
  
  print(nrow(all_votes))
  
  all_votes_small <- filter(all_votes, 
                            vote_date < ymd("2017-01-01"),
                            vote_date > ymd("2016-01-01"))
  
  arp_ideal_data <- id_make(score_data = all_votes,
                            outcome_disc="vote_choice",
                            person_id="mp_id",
                            item_id="vote_id",
                            time_id="vote_date",
                            group_id="mp_bloc_name",remove_cov_int = F,
                            person_cov=~change*mp_bloc_name)
  
  # arp_ideal_data@person_cov <- c(arp_ideal_data@person_cov[1],arp_ideal_data@person_cov[10:15])
  # arp_ideal_data@score_matrix <- select(arp_ideal_data@score_matrix,item_id:change,
  #                                       `change:mp_bloc_nameAucun bloc`:discrete)

  # arp_ideal_data@person_cov <- c(arp_ideal_data@person_cov[1],arp_ideal_data@person_cov[10:17])
  # arp_ideal_data@score_matrix <- select(arp_ideal_data@score_matrix,item_id:change,
  
  
  estimate_all <- id_estimate(arp_ideal_data,
                              use_groups = F,
                              restrict_ind_high=c("554ced8712bdaa5df2537688","569416d212bdaa5ee3796068",
                              "59b287314f24d0311313bfff","5ba234a14f24d03ba3d842a2"),
                              restrict_ind_low=c("5603199812bdaa20aa5b4907",
                                                 "5c9cf4aa4f24d0572feb077c","57a31e71cf44122088ceed2c",
                                                 "5866afa7cf44121f3e63b001"),
                              #restrict_ind_high="57a31e71cf44122088ceed2c",
                              #restrict_ind_low="57a31e71cf44122088ceed31",
                              const_type="items",
                              #restrict_ind_high= "Nahda",
                              #restrict_ind_low="Front Populaire",
                              model_type=2,map_over_id = "persons",
                              vary_ideal_pts = 'splines',
                              spline_degree=2,adapt_delta=0.95,
                              nchains = 4,
                              ncores = parallel::detectCores(),
                              fixtype='prefix',niters = 500,
                              warmup=500,id_refresh=10)
   
  
  
  saveRDS(estimate_all,paste0('/lustre/scratch/rkubinec/arp/estimate_all_ar3_full_rv',model_type,'.rds'))
  

