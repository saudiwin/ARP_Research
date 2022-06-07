# Analysis of bills

require(bawsala)
require(ggplot2)
require(dplyr)
require(tidyr)
check_summary <- rstan::extract(sample_fit)
betas <- check_summary$B_yes
betas2 <- check_summary$B_abs
sigmas <- check_summary$sigma_adj
sigmas2 <- check_summary$sigma_abs_open
steps <- check_summary$steps_votes
avg_particip <- check_summary$avg_particip
# convert intercepts to difficulty parameters

betas_abst <- sapply(1:ncol(betas),function(x)  {
  y <- (betas[,x] + steps[,1]) / sigmas[,x] 
  return(y)
  })
betas_yes <- sapply(1:ncol(betas),function(x)  {
  y <- (betas[,x] + steps[,2]) / sigmas[,x] 
  return(y)
})

betas2 <- sapply(1:ncol(betas2),function(x)  {
  y <- (betas2[,x]- mean(mean(avg_particip)*participation$particip_rate))/ sigmas2[,x]
  return(y)
})


betas <- data_frame(betas_yes_mean=apply(betas_yes,2,mean),
                    betas_yes_sd=apply(betas_yes,2,sd),
                    betas_no_mean=apply(betas_abst,2,mean),
                    betas_no_sd=apply(betas_abst,2,sd))
betas2 <- data_frame(betas_absent_mean=apply(betas2,2,mean),
                     betas_absent_sd=apply(betas2,2,sd))
sigmas <- data_frame(sigmas_ord_mean=apply(sigmas,2,mean),
                     sigmas_ord_sd=apply(sigmas,2,sd))
sigmas2 <- data_frame(sigmas_abs_mean=apply(sigmas2,2,mean),
                     sigmas_abs_sd=apply(sigmas2,2,sd))

all_params <- bind_cols(betas,betas2,sigmas,sigmas2) %>% 
  mutate(bill_num=colnames(vote_matrix))

horra_vote_share <- gather(cleaned[[legislature]],bill_num,vote,matches('Bill')) %>% 
  group_by(bill_num,bloc) %>% summarize(absent=sum(vote==4),no=sum(vote==1),yes=sum(vote==3),
                                        abstain=sum(vote==2))

horra_vote_share <- left_join(horra_vote_share,all_params,by='bill_num') %>% 
  mutate(bloc_l= as.character(factor(bloc,labels=c('Afek Tounes','Aucun','Horra','Social-Democrat',
                                      'FP','Nahda','Nidaa Tounes','UPL'))),
         bloc_l=substr(bloc_l,1,2))

horra_vote_share %>% filter(bloc=='Bloc Al Horra',betas_yes_mean>-20,betas_yes_mean<20) %>% 
  ggplot(aes(y=yes,x=betas_ord_mean)) + geom_point() + theme_minimal() + stat_smooth(method='lm')

horra_vote_share %>% filter(bloc=='Bloc Al Horra',betas_yes_mean>-20,betas_yes_mean<20) %>% 
  ggplot(aes(y=no,x=betas_ord_mean)) + geom_point() + theme_minimal() + stat_smooth(method='lm')

outplot <- plot_IRT(cleaned=cleaned,
                    stan_obj=sample_fit,
                    legislature="arp_votes",
                    plot_param='L_open',ggplot=TRUE,
                    text_labsize = 3) 
discrim_horra <- filter(horra_vote_share,betas_yes_mean > -.4,betas_yes_mean< 0,
                        betas_yes_sd<0.07,bloc=='Bloc Al Horra')

all_plots <- discrim_horra %>%   mutate(betas_absent_mean=ifelse(betas_absent_mean<(-1),NA,betas_absent_mean)) %>% 
  gather(bill_pts,estimate,betas_yes_mean,betas_no_mean,betas_absent_mean) %>%
  do(combine_plot=outplot + geom_vline(data=.,aes(xintercept=estimate,linetype=bill_pts)) +
       scale_linetype(breaks=c('betas_no_mean','betas_yes_mean','betas_absent_mean'),
                      labels=c('No','Yes','Absent')) + labs(linetype='Bill Ideal Pt',title=.$bill_num[1]))

# png(filename='output_graphs/horra_bills_discrim.png',width=4000,height=4000,res=200)
# outplot + geom_vline(data=discrim_horra,aes(xintercept=betas_ord_mean)) 
# dev.off()

purrr::walk(1:length(all_plots$combine_plot),function(x) {
  png(filename=paste0('output_graphs/',all_plots$bill_num[x],'_plot.png'),
      width=4000,height=4000,res=200)
  print(all_plots$combine_plot[[x]])
  dev.off()
  discrim_horra %>% filter(bill_num==all_plots$bill_num[x]) %>% 
  write.csv(.,file = paste0('output_graphs/',all_plots$bill_num[x],'_data.csv'))
})

write.csv(discrim_horra,file='data/all_discrim_data.csv')

