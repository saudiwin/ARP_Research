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
  
  all_votes <- readRDS("data/all_votes.rds")
  
  check_bills1 <- group_by(all_votes,
                           vote_id,mp_bloc_name,vote_choice,change,vote_date) %>% count %>% 
    filter(!is.na(vote_choice)) %>% 
    group_by(change,vote_id,vote_date) %>% 
    summarize(diff=mean(sqrt((n[mp_bloc_name=="Front Populaire" & vote_choice=="YES"] - n[mp_bloc_name=="Nahda" & vote_choice=="YES"])^2)),
              R_vote=n[mp_bloc_name=="Nahda" & vote_choice=="YES"],
              D_vote=n[mp_bloc_name=="Front Populaire" & vote_choice=="YES"],
              polarity = sign(R_vote - D_vote)) %>% 
    ungroup %>% 
    group_by(change,polarity) %>% 
    filter(diff > quantile(diff, .85)) %>% 
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
                              restrict_ind_high=c("5d42e9b04f24d01b78a3817c",
                              "57a1b247cf4412208bceed66","585281c5cf44121f3e63aee2"),
                              restrict_ind_low=c("5c9cf4aa4f24d0572feb077c","57a31e71cf44122088ceed2c",
                                                 "5866afa7cf44121f3e63b001"),
                              #restrict_ind_high="57a31e71cf44122088ceed2c",
                              #restrict_ind_low="57a31e71cf44122088ceed31",
                              const_type="items",
                              #restrict_ind_high= "Nahda",
                              #restrict_ind_low="Front Populaire",
                              model_type=3,map_over_id = "persons",
                              vary_ideal_pts = 'splines',
                              spline_degree=3,adapt_delta=0.95,
                              nchains = 2,
                              ncores = parallel::detectCores(),
                              fixtype='prefix',niters = 500,
                              warmup=500,id_refresh=10)
   
  
  
  saveRDS(estimate_all,'/scratch/rmk7/arp/estimate_all_ar3_full_rv.rds')
  
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
  
  estimate_all1 <- readRDS("data/estimate_all_ar1_full_rv.rds")
  estimate_all2 <- readRDS("data/estimate_all_ar2_full_rv.rds")
  estimate_all3 <- readRDS("data/estimate_all_ar3_full_rv.rds")
  p2 <- id_plot_legis_dyn(estimate_all2,person_plot=F,use_ci = F) +
    geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
               linetype=2)
  p1 <- id_plot_legis_dyn(estimate_all1,person_plot=F,use_ci = F) +
    geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
               linetype=2)
  p3 <- id_plot_legis_dyn(estimate_all3,person_plot=F,use_ci = F) +
    geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
               linetype=2)
    # scale_y_continuous(labels=c('More\nSecular','-1.0','-0.5','0.0','0.5','More\nIslamist'),
    #                    breaks=c(-1.5,-1.0,-0.5,0.0,0.5,1.0)) +
    #facet_wrap(~group_id,scales='free_y')
  
  library(patchwork)
  
  p1 + p2 + p3 & theme(text=element_text(family=""))
  
  ggsave('party_over_time.png',width=12,height=6)
  
  require(bayesplot)
  
  library("ggplot2")
  library("hrbrthemes")
  #> NOTE: Either Arial Narrow or Roboto Condensed fonts are required to use these themes.
  #>       Please use hrbrthemes::import_roboto_condensed() to install Roboto Condensed and
  #>       if Arial Narrow is not on your system, please see https://bit.ly/arialnarrow
  
  
  options(hrbrthemes.loadfonts = TRUE)
  require(posterior)
  require(tidybayes)
  require(tidyverse)
  
  get_covs <- bind_draws(estimate_all1@stan_samples$draws("legis_x"),
                         estimate_all2@stan_samples$draws("legis_x"),
                         estimate_all3@stan_samples$draws("legis_x"),
                         along="chain")
  
  get_covs <- as_draws_df(get_covs) %>% 
    gather(key="variable",value="estimate",matches('legis')) %>% 
    mutate(variable=factor(variable,levels=c("legis_x[1]",
                                             "legis_x[2]",
                                             "legis_x[3]",
                                             "legis_x[4]",
                                             "legis_x[5]",
                                             "legis_x[6]",
                                             "legis_x[7]",
                                             "legis_x[8]",
                                             "legis_x[9]",
                                             "legis_x[10]",
                                             "legis_x[11]",
                                             "legis_x[12]",
                                             "legis_x[13]"),
                           labels=estimate_all1@score_data@person_cov))
  
  get_covs %>% 
    ggplot(aes(x=estimate, y=variable)) +
    stat_halfeye() + geom_vline(xintercept=0) +
    ggtitle("Effect of Carthage on Party-Level Ideal Points")
  
  ggsave("cov_effect.png")
  
  # id_plot_cov(estimate_all)
  # id_plot_cov(estimate_all,filter_cov = c('change:blocHorra','change'))
  id_plot_legis_var(estimate_all2)
  
  # pull out bill discrimination parameters 
  
  all_params <- summary(estimate_all2)
  just_discrim <- filter(all_params,grepl(pattern = 'sigma_reg_free',x=`Parameter Name`)) %>% 
    mutate(abs_score=abs(`Posterior Median`),
           index=as.numeric(str_extract(`Parameter Name`,'[0-9]+'))) %>% 
    arrange(desc(abs_score))
  group_ids <- select(estimate_all2@score_data@score_matrix,item_id) %>% 
    mutate(index=as.numeric(item_id)) %>% 
    distinct
  
  just_discrim <- left_join(just_discrim,group_ids,'index')
  
  all_out <- xtable(select(just_discrim,
                           Vote='item_id',
                           `Discrimination Score`="Posterior Median"))
  print(all_out,type='latex',file='discrim_bill.tex')
  
  
  
}


