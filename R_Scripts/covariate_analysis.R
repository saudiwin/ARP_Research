# let's look at covariate effects of the ARP model

library(tidyverse)
library(ggplot2)
library(idealstan)
library(ggthemes)
library(posterior)

arp_est <- readRDS("data/estimate_all_ar3_full_rv.rds")

# map covariate effects

cov_estimates <- summarize_draws(arp_est@stan_samples$draws("legis_x")) %>% 
  mutate(cov_names=arp_est@score_data@person_cov,
         cov_names=fct_recode(cov_names,
           "Baseline: Afek Tounes" = "change",
           "Carthage X Independent" = "change:mp_bloc_nameAucun bloc",
           "Carthage X Front Populaire" = "change:mp_bloc_nameFront Populaire",
           "Carthage X Horra" = "change:mp_bloc_nameHorra",
           "Carthage X Nahda" = "change:mp_bloc_nameNahda",
           "Carthage X Nidaa Tounes" = "change:mp_bloc_nameNidaa Tounes",
           "Carthage X Social Democrate" = "change:mp_bloc_nameSocial-D<U+00E9>mocrate",
           "Carthage X Tahya Tounes" = "change:mp_bloc_nameTahya Tounes",
           "Carthage X UPL" = "change:mp_bloc_nameUnion Patriotique Libre",
           "Independent" = "mp_bloc_nameAucun bloc",
           "Front Populaire" = "mp_bloc_nameFront Populaire",
           "Horra" = "mp_bloc_nameHorra",
           "Nahda" = "mp_bloc_nameNahda",
           "Nidaa Tounes" = "mp_bloc_nameNidaa Tounes",
           "Social Democrate" = "mp_bloc_nameSocial-D<U+00E9>mocrate",
           "Tahya Tounes" = "mp_bloc_nameTahya Tounes",
           "UPL" = "mp_bloc_nameUnion Patriotique Libre"
         )) %>% 
  filter(grepl(x=cov_names, pattern="Carthage"),!grepl(x=cov_names,
                                                       pattern="Tahya"))

# plot

cov_estimates %>% 
  ggplot(aes(y=mean,
             x=cov_names)) +
  geom_pointrange(aes(ymin=q5,ymax=q95)) +
  ggtitle("Effects of Carthage Agreement on Party-level Ideal Points",
          subtitle="Positive Values Associated with Islamism and Negative with Secularism") +
  theme_tufte() +
  geom_hline(yintercept=0, linetype=2) +
  annotate("text",x=c("Carthage X Nidaa Tounes","Carthage X Nidaa Tounes"),
            y=c(-2,2.5),label=c("More Secular","More Islamist")) +
  labs(x="",y="Effect on Ideal Point Scores",
       caption=stringr::str_wrap("Plot shows average changes post-Carthage Agreement in party-level ideal point scores based on voting behavior for legislation. Positive values indicate movement in an Islamist direction (Nahda) and negative values indicate movement in a secularist direction (Front Populaire).")) +
  coord_flip()

ggsave("cov_estimates.jpg",width=7,height=4)

all_params <- summary(estimate_all3)
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


# marginal effect analysis

# need new data

all_votes <- readRDS("data/all_votes.rds")

eps <- 1e-4

new_data1 <- mutate(all_votes, change=0) 

new_data2 <- all_votes

l_full <- arp_est@stan_samples$draws("L_full")

draws <- sample(1:dim(l_full)[1], 200)

arp_est_pred1 <- id_post_pred(arp_est,newdata=new_data1,
                               use_cores=floor(parallel::detectCores()/2),
                               type="epred",
                               draws=draws)
arp_est_pred2 <- id_post_pred(arp_est,newdata=new_data2,
                               use_cores=floor(parallel::detectCores()/2),
                               type="epred",
                               draws=draws)

saveRDS(arp_est_pred1, paste0("/scratch/rmk7/arp_pred1_",m,".rds"))
saveRDS(arp_est_pred2, paste0("/scratch/rmk7/arp_pred2_",m,".rds"))

# walk over both predictions to get item and overall effects
# AMEs per item

c1 <- purrr::map2(arp_est_pred1,
                  arp_est_pred2,
                  function(small,big) {
                    
                    # difference the effects
                    
                    (big - small) / eps
                    
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

c3 <- lapply(c1, function(mat) {
  
  
  apply(mat, 1, mean)
  
})

old_style_version <- tibble(item_id=sapply(c1, function(c) unique(attr(c, "data")$item_id)),
                            mean_est=sapply(c3, mean),
                            low_est=sapply(c3, quantile, .05),
                            high_est=sapply(c3, quantile, .95))

# merge in some original data
to_merge <- mutate(arp_est@score_data@score_matrix, 
                   item_orig=item_id,
                   person_orig=person_id,
                   person_id=as.numeric(person_id),
                   item_id=as.numeric(item_id)) %>% 
  select(person_id, item_id, group_id,item_orig, person_orig) %>% 
  distinct
