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

# legislative data 
# provided by Al-Bawsala

run_model <- T

if(run_model) {
  
  all_votes <- readr::read_csv('data/parliament_observatory_al_bawsala/Votes_M01.csv')
  
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
                       ordered_id=3)
  
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
                               `Bloc Démocrate`="Social-Démocrate",
                               `Tahya Tounes`="Nidaa Tounes"))
  
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
                            group_id="mp_bloc_name",remove_cov_int = T,
                            person_cov=~change*mp_bloc_name)
  
  arp_ideal_data@person_cov <- c(arp_ideal_data@person_cov[1],arp_ideal_data@person_cov[10:15])
  arp_ideal_data@score_matrix <- select(arp_ideal_data@score_matrix,item_id:change,
                                        `change:mp_bloc_nameAucun bloc`:discrete)
  
  
  estimate_all <- id_estimate(arp_ideal_data,
                              use_groups = T,
                              restrict_ind_high="5aeae5b84f24d02328a2f1aa",
                              restrict_ind_low="58528919cf44121f3e63aee5",
                              #restrict_ind_high="57a31e71cf44122088ceed2c",
                              #restrict_ind_low="57a31e71cf44122088ceed31",
                              const_type="items",time_var=12000,
                              #restrict_ind_high= "Nahda",
                              #restrict_ind_low="Front Populaire",
                              model_type=3,
                              vary_ideal_pts = 'random_walk',nchains = 2,
                              ncores = parallel::detectCores(),
                              fixtype='prefix',niters = 250,
                              warmup=250,id_refresh=10)
  
  
  
  saveRDS(estimate_all,'/scratch/rmk7/arp/estimate_all_ar1_full.rds')
  
} else {
  
  all_votes <- readr::read_csv('data/parliament_observatory_al_bawsala/Votes_M01.csv')
  
  all_votes <-  mutate(all_votes,mp_bloc_name=recode(mp_bloc_name,
                                                     `Afek Tounes et l'appel des tunisiens à l'étranger`="Afek Tounes",
                                                     `Bloc Social-Démocrate`="Social-Démocrate",
                                                     `Bloc Al Horra du Mouvement Machrouu Tounes`="Horra",
                                                     `Mouvement Ennahdha`="Nahda",
                                                     `Mouvement Nidaa Tounes`='Nidaa Tounes'),
                       vote_choice=factor(vote_choice,levels=c("NO","ABSTAIN","YES")),
                       change=as.numeric(vote_date>ymd("2016-07-13")),
                       ordered_id=3)
  
  unam_votes <- group_by(all_votes, vote_id) %>% 
    summarize(unan=all(vote_choice[!is.na(vote_choice)]=="YES") || all(vote_choice[!is.na(vote_choice)]=="NO"))
  
  all_votes <- anti_join(all_votes,filter(unam_votes,unan),by="vote_id")
  
  # to run this model, need to remove those parties over which the covariate is not defined
  all_votes <- filter(all_votes,mp_bloc_name %in% c('Nahda',
                                                    'Nidaa Tounes',
                                                    'Front Populaire',
                                                    "Social-Démocrate",
                                                    "Afek Tounes",
                                                    "Union Patriotique Libre",
                                                    "Bloc Démocrate",
                                                    "Aucun bloc"))
  
  bill_stat <- all_votes %>% 
    group_by(vote_id) %>% 
    count(vote_choice)
  
  bill_stat_nahda <- filter(all_votes, mp_bloc_name=="Nahda") %>% 
    group_by(vote_id) %>% 
    count(vote_choice)
  
  bill_stat <- left_join(bill_stat,bill_stat_nahda, by=c("vote_id","vote_choice"))
  
  bill_sum <- group_by(bill_stat, vote_id) %>% 
    summarize(n_yes_nahda=n.y[vote_choice=='YES']/sum(n.y,na.rm=T),
              n_yes_total=n.x[vote_choice=='YES']/sum(n.x,na.rm=T),
              n_no_nahda=n.y[vote_choice=='NO']/sum(n.y,na.rm=T),
              n_no_total=n.x[vote_choice=='NO']/sum(n.x,na.rm=T)) %>% 
    filter(!is.na(n_yes_nahda),!is.na(n_no_total))
  
  yes_votes <- filter(bill_sum, n_yes_nahda>.9, n_no_total>.1)
  no_votes <- filter(bill_sum, n_no_nahda>.7, n_yes_total>.05)
  
  # we need to complete the data for horra and others who aren't in every time point
  # this is for group covariate plotting
  # all_votes <- all_votes %>% complete(law_date,nesting(bloc,change),fill=list(change=0))
  
  # first run an AR(1) model with covariates
  
  print(nrow(all_votes))
  
  estimate_all <- readRDS("data/estimate_all_ar1_full.rds")
  
  id_plot_legis_dyn(estimate_all,person_plot=F,use_ci = F) +
    geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
               linetype=2) +
    # scale_y_continuous(labels=c('More\nSecular','-1.0','-0.5','0.0','0.5','More\nIslamist'),
    #                    breaks=c(-1.5,-1.0,-0.5,0.0,0.5,1.0)) +
    facet_wrap(~group_id,scales='free_y')
  
  ggsave('party_over_time.png')
  
  id_plot_cov(estimate_all)
  id_plot_cov(estimate_all,filter_cov = c('change:blocHorra','change'))
  id_plot_legis_var(estimate_all)
  
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
  
  
  
}


