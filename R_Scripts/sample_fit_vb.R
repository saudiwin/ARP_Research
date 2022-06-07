sample_fit_vb <- vb(stan_model(file = 'R_Scripts/ordinal_split_absence_nofix.stan'),data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                                                  bb=bill_points,
                                                  fixed_bills=if(identify=='ref_bills') {
                                                    length(to_fix$final_constraint)
                                                  } else if(identify=='ref_discrim') {
                                                    opp_num+gov_num
                                                  },
                                                  bill_pos=to_fix$constraint_num,
                                                  opp_num=opp_num,gov_num=gov_num,particip=participation$particip_rate),
                 algorithm='meanfield')

check_summary_vb <- summary(sample_fit)[[1]]
sigmas_vb <- check_summary_vb %>% as_data_frame %>% mutate(params=row.names(check_summary_vb)) %>% 
  filter(grepl('sigma_adj',x = params)) 
sigmas_vb <- sigmas_vb %>%  arrange(mean)
to_constrain <- sigmas_vb$params[1:60]
sigmas_vb <- check_summary_vb %>% as_data_frame %>% mutate(params=row.names(check_summary_vb)) %>% 
  filter(grepl('sigma_adj',x = params)) 
constrain_sigmas <- data_frame(params=c(sigmas_vb$params[!(sigmas_vb$params %in% to_constrain)],
                                        to_constrain))
keep_cols_vb_digits <- as.numeric(stringr::str_extract(to_constrain,'[0-9]+'))
keep_cols_vb_names <- colnames(vote_matrix)[keep_cols_vb_digits]
sum(keep_cols %in% keep_cols_vb_names)

constrain_sigmas <- left_join(constrain_sigmas,sigmas_vb,by='params')
sigma_starts <- constrain_sigmas$mean[1:(nrow(sigmas_vb)-60)]
sigma_gov_starts <- constrain_sigmas$mean[(nrow(sigmas_vb)-59):nrow(sigmas_vb)]
L_free_starts <- check_summary_vb %>% as_data_frame %>% mutate(params=row.names(check_summary_vb)) %>% 
  filter(grepl('L_free',x = params)) %>% dplyr::select(mean)
B_yes_starts <- check_summary_vb %>% as_data_frame %>% mutate(params=row.names(check_summary_vb)) %>% 
  filter(grepl('B_yes',x = params)) %>% dplyr::select(mean)
B_abs_starts <- check_summary_vb %>% as_data_frame %>% mutate(params=row.names(check_summary_vb)) %>% 
  filter(grepl('B_abs',x = params)) %>% dplyr::select(mean) 
cutpoints <- check_summary_vb %>% as_data_frame %>% mutate(params=row.names(check_summary_vb)) %>% 
  filter(grepl('steps',x = params)) %>% dplyr::select(mean) 
particip <- check_summary_vb %>% as_data_frame %>% mutate(params=row.names(check_summary_vb)) %>% 
  filter(grepl('particip',x = params)) %>% dplyr::select(mean) 
sigma_abs <- check_summary_vb %>% as_data_frame %>% mutate(params=row.names(check_summary_vb)) %>% 
  filter(grepl('sigma_abs_open',x = params)) %>% dplyr::select(mean) 

starts <- list(sigma=sigma_starts,sigma_gov=sigma_gov_starts,sigma_abs_open=sigma_abs$mean,
               steps_votes=cutpoints$mean,avg_particip=particip$mean)

saveRDS(starts,'vb_starts.rds')
