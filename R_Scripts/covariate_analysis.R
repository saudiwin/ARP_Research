# let's look at covariate effects of the ARP model

library(tidyverse)
library(ggplot2)
library(idealstan)
library(ggthemes)
library(posterior)
library(tinytable)

arp_est <- readRDS("data/estimate_all_ar3_full_rv1.rds")

# make some distributions

id_plot_legis_dyn(arp_est,
                  group_color=F,person_plot=F,text_size_label=8,use_ci = F,plot_text = F,
                highlight="Ameur_Laraiedh") +
  labs(y="",x="") +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  geom_hline(yintercept = 0,linetype=3,color="red") +
  annotate(geom='text',x=ymd('2017-01-30'),y=22,label='Carthage\nAgreement',size=3.5) +
  # annotate(geom='text',x=ymd('2014-12-2'),y=-17,label='New\nParliament',size=3.5) +
  annotate(geom='text',x=ymd('2017-12-30'),y=4,label='Ameur Laraiedh\n(Nahda Party)',size=3) +
   scale_y_continuous(labels=c('Government','0.0','Opposition'),
                      breaks=c(20,0.0,-20)) +
  scale_color_discrete(guide='none') + 
  scale_x_date(date_breaks = '1 year',
               date_labels='%Y')

ggsave('party_over_time_arp.png',width=6,height=4)

# whether or not to re-run marginal effect estimation

run_margins <- F

# map covariate effects

cov_estimates <- summarize_draws(arp_est@stan_samples$draws("legis_x")) %>% 
  mutate(cov_names=arp_est@score_data@person_cov,
         cov_names=fct_recode(cov_names,
           "Nahda Post-Carthage" = "person_change",
           "Carthage X Independent" = "person_change:mp_bloc_nameAucun bloc",
           "Carthage X Front Populaire" = "person_change:mp_bloc_nameFront Populaire",
           "Carthage X Horra" = "person_change:mp_bloc_nameHorra",
           "Carthage X Afek Tounes" = "person_change:mp_bloc_nameAfek Tounes",
           "Carthage X Nidaa Tounes" = "person_change:mp_bloc_nameNidaa Tounes",
           "Carthage X Social Democrate" = "mp_bloc_nameSocial-Démocrate",
           "Carthage X Tahya Tounes" = "person_change:mp_bloc_nameTahya Tounes",
           "Carthage X UPL" = "person_change:mp_bloc_nameUnion Patriotique Libre",
           "Independent" = "mp_bloc_nameAucun bloc",
           "Front Populaire" = "mp_bloc_nameFront Populaire",
           "Horra" = "mp_bloc_nameHorra",
           "Afek Tounes" = "mp_bloc_nameAfek Tounes",
           "Nidaa Tounes" = "mp_bloc_nameNidaa Tounes",
           "Social Democrate" = "mp_bloc_nameSocial-Démocrate",
           "Tahya Tounes" = "mp_bloc_nameTahya Tounes",
           "UPL" = "mp_bloc_nameUnion Patriotique Libre"
         )) %>% 
  filter(grepl(x=cov_names, pattern="Carthage"),!grepl(x=cov_names,
                                                       pattern="Tahya|Nahda"))

# plot

cov_estimates %>% 
  ggplot(aes(y=mean,
             x=cov_names)) +
  geom_pointrange(aes(ymin=q5,ymax=q95),colour="black") +
  ggtitle("Effects of Carthage Agreement on Party-level Ideal Points",
          subtitle="Positive Values Associated with Islamism and Negative with Secularism") +
  ggthemes::theme_clean() +
  geom_hline(yintercept=0, linetype=2,colour="black") +
  annotate("text",x=c("Carthage X Nidaa Tounes","Carthage X Nidaa Tounes"),
            y=c(-0.5,2),label=c("More\nSecular","More\nIslamist"),
           colour="black") +
  labs(x="",y="Effect on Ideal Point Scores",
       caption=stringr::str_wrap("Plot shows average changes post-Carthage Agreement in party-level ideal point scores based on voting behavior for legislation. Positive values indicate movement in an Islamist direction (Nahda) and negative values indicate movement in a secularist direction (Front Populaire).")) +
  coord_flip()

ggsave("cov_estimates.jpg",width=7,height=4)

cov_estimates %>% 
  select(`Covariate`="cov_names",`5% Low`="q5",`Point Estimate`="mean",`95% High`="q95") |> 
  tt(caption="Effects of Carthage Agreement on Party-level Ideal Points\\label{tab:arp_effect}",digits=2,
      notes=c("Table shows estimates of party-level changes in ideal points following the implementation of the Carthage Agreement. Positive scores indicate movement in a pro-government direction. Estimates represent empirical means and quantiles of the joint posterior distribution.")) |> 
  save_tt("arp_cov.tex",overwrite=T)

all_params <- summary(arp_est)
just_discrim <- filter(all_params,grepl(pattern = 'sigma_reg_free',x=`Parameter Name`)) %>% 
  mutate(abs_score=abs(`Posterior Median`),
         index=as.numeric(str_extract(`Parameter Name`,'[0-9]+'))) %>% 
  arrange(desc(abs_score))
group_ids <- select(arp_est@score_data@score_matrix,item_id) %>% 
  mutate(index=as.numeric(item_id)) %>% 
  distinct

just_discrim <- left_join(just_discrim,group_ids,'index')

# marginal effect analysis

# need new data

all_votes <- readRDS("data/all_votes.rds") %>% 
  mutate(vote_choice=na_if(vote_choice, "ABSTAIN"),
         mp_bloc_name=fct_relevel(factor(mp_bloc_name),"Nahda"))

if(run_margins) {

  eps <- 1e-4

new_data1 <- mutate(all_votes, change=1 - change) 

new_data2 <- all_votes

l_full <- arp_est@stan_samples$draws("L_full")

#draws <- sample(1:dim(l_full)[1], 200)

#saveRDS(draws,"data/draws.rds")

draws <- readRDS("data/draws.rds")

arp_est_pred1 <- id_post_pred(arp_est,newdata=new_data1,
                               use_cores=floor(parallel::detectCores()/2),
                               type="epred",
                               draws=draws)
arp_est_pred2 <- id_post_pred(arp_est,newdata=new_data2,
                                use_cores=2,
                               #use_cores=floor(parallel::detectCores()/2),
                               type="epred",
                               draws=draws)

saveRDS(arp_est_pred1, "data/arp_pred1.rds")
saveRDS(arp_est_pred2, "data/arp_pred2.rds")

# walk over both predictions to get item and overall effects
# AMEs per item

c1 <- purrr::map2(arp_est_pred1[[1]],
                  arp_est_pred2[[1]],
                  function(small,big) {
                    
                    # difference the effects
                    
                    (big - small)
                    
                  })

c2 <- lapply(c1, function(mat) {
  
  
  out_data <- attr(mat, "data")
  colnames(mat) <- out_data$person_id
  
  as_tibble(mat) %>% 
    mutate(draws=1:n(),
           item_id=unique(out_data$item_id)) %>% 
    gather(key="person_id",value="estimate",-draws,-item_id) %>% 
    mutate(person_id=as.numeric(person_id),
           estimate=as.numeric(estimate))
  
}) %>% bind_rows
  
  to_merge <- mutate(arp_est@score_data@score_matrix, 
                   item_orig=item_id,
                   time_id=time_id,
                   person_orig=person_id,
                   person_id=as.numeric(person_id),
                   item_id=as.numeric(item_id),) %>% 
  select(person_id, item_id, group_id,item_orig, person_orig,time_id) %>% 
  distinct

c2 <- left_join(c2, to_merge, 
                by=c("item_id","person_id"))

saveRDS(c2,"data/c2.rds")

# get effect separately by democrats/republicans

by_party <- group_by(c2, draws, group_id, item_id, item_orig,time_id) %>% 
  summarize(mean_est1=mean(estimate)) %>% 
  group_by(group_id, item_id, item_orig,time_id) %>% 
  summarize(mean_est=mean(mean_est1),
            low_est=quantile(mean_est1, .05),
            high_est=quantile(mean_est1, .95))

# merge in item discrimination

item_discrim <- filter(arp_est@summary,
                       grepl(x=variable, pattern="sigma\\_reg\\_free")) %>% 
  mutate(item_id=as.numeric(str_extract(variable, "[0-9]+")))

by_party <- left_join(by_party,
                      select(item_discrim, median, item_id))

saveRDS(by_party,"data/by_party.rds")

}

by_party <- readRDS("data/by_party.rds")

check_date <- filter(arp_est@score_data@score_matrix, person_change==1) %>% 
  summarize(post_carthage=min(time_id))

by_party %>% 
  ungroup %>% 
  filter(time_id >= check_date$post_carthage) %>% 
  mutate(group_id=factor(group_id, labels=levels(all_votes$mp_bloc_name)),
              group_id=case_match(group_id,
                             "Nahda"~"Baseline: Nahda",
                             .default= group_id),
         group_id=fct_relevel(group_id, "Baseline: Nahda")) %>% 
  filter(group_id != "Tahya Tounes") |> 
  ggplot(aes(y=mean_est,
             x=reorder(item_id,mean_est))) +
  geom_linerange(aes(ymin=low_est,
                     ymax=high_est,
                     colour=`median`)) +
  facet_wrap(~group_id) +
  ggthemes::theme_tufte() + 
  scale_colour_viridis_c(name="Vote\nPolarization",labels=c("Pro\nOpposition","0","Pro\nGovernment"),breaks=c(-0.8, 0, 0.8)) +
  coord_flip() +
  labs(y="Marginal Change in Probability of Voting",
       x="Rollcalls") +
  geom_hline(yintercept=0,linetype=2,colour="black") +
  theme_tufte() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),legend.position = "top",legend.title.position = "top")

ggsave("post_carthage_marginal_eff.pdf",width=6,height=4)
ggsave("post_carthage_marginal_eff.jpg",width=6,height=4)
