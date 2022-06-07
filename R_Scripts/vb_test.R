
#this is useful for finding a suitable posterior mode
# VB works better than full Bayes at finding a mode very quickly
# Assuming that we don't care about the other posterior modes because they are reflections

compiled_model <- stan_model(file='R_Scripts/grm_split_absence_nofix.stan',model_name="Find Posterior Modes")
post_modes <- vb(object=compiled_model,data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                                                   bb=bill_points,particip=participation$particip_rate),
                 algorithm='meanfield')

lookat_params <- rstan::extract(post_modes,permuted=FALSE)
lookat_params <- lookat_params[,1,]
sigmas_est <- lookat_params[,grepl('sigma\\[',colnames(lookat_params))]
# sigmas2_est <- lookat_params[,grepl('sigma_abs\\[',colnames(lookat_params))]
sigmas_est <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
  summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05))
# sigmas2_est <- sigmas2_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
#   summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05))

sigmas <- arrange(sigmas_est,avg)
keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:60])
keep_cols <- colnames(vote_matrix)[keep_cols]
saveRDS(keep_cols,'keep_cols_gov.rds')

# Create starts 

start_params <- rstan::extract(post_modes)
start_params <- lapply(start_params, function(x) {
  if(is.matrix(x)==TRUE) {
    x <- colMeans(x)
  } else {
    x <- mean(x)
  }
  return(as.numeric(x))
})
keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:60])
start_params$sigma_gov <- start_params$sigma[keep_cols]
start_params$sigma <- start_params$sigma[-keep_cols]
saveRDS(start_params, 'vb_starts.rds')
