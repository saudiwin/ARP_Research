# Model of Tunisian legislative ideal points
# Data provided by NGO Bawsala
# Analysis by Robert Kubinec and Sharan Grewal
# July 5th, 2016


require(rstan)
require(bawsala)
require(bayesplot)
require(tidyr)
require(dplyr)
require(archivist)

#CONTROL PANEL

# Keep legislators with have voted on at least this many bills
keep_legis <- 1
# Use only the parties in the subset_party variable?
use_subset <- FALSE
subset_party <- c("Bloc Al Horra","Mouvement Nidaa Tounes",'Front Populaire')
# Check out partial credit IRT
categorical <- FALSE

# What type of identification to use
identify <- 'ref_discrim'

# Which of the legislatures to use-- ARP or ANC
use_both <- FALSE
legislature <- "arp_votes"
# Use variational inference? Faster, but less accurate
use_vb <- FALSE
# Convert absences to a separate category in ordinal regression?
use_nas <- TRUE
# Split absences by whether absences are for/against party votes? (note: indicates the use of a different ordinal model)
split_absences <- TRUE
#Reference bill for absences constraint
ref_absence <- 'Bill_3977'
# Which dataset to use? Put 1 for binary, 2 for abstain, 3 for ordinal
to_run <- 3
# Use only a sample of bills/legislators?
sample_it <- FALSE
# Legislator to use as a reference for IRT model
legislator<- "Bochra Belhaj Hamida"


# clean and load data for the kind of analysis (ordinal v. binary)




# Use Bouchra's votes to filter out those particular bills that are useful for pinning the scale

cleaned <- clean_data(keep_legis=keep_legis,use_subset=use_subset,subset_party=subset_party,
                    use_both=use_both,refleg=legislator,
                    legis=1,use_vb=use_vb,use_nas=use_nas,to_run=to_run,sample_it=sample_it)

# to_fix  <- fix_bills(legislator=legislator,party=subset_party,
#                      vote_data=cleaned,legislature=legislature)

to_fix <- fix_bills_discrim(opp='Front Populaire',gov="Mouvement Nidaa Tounes",
                            vote_data=cleaned,legislature=legislature,to_run=to_run,use_nas=use_nas)

# Prepare matrix for model 

new_vote_matrix <- apply(readRDS('vote_matrix.rds'),2,function(x) {
  x <- ifelse(x>3,1,-1)
})
require(emIRT)
abs_model <- binIRT(.rc=list(votes=new_vote_matrix),
                    .starts=getStarts(nrow(new_vote_matrix),
                                      ncol(new_vote_matrix),1),
                    .priors=makePriors(nrow(new_vote_matrix),
                                       ncol(new_vote_matrix),1))
discrims <- data_frame(discrims=abs_model$means$beta[,1],bills=colnames(new_vote_matrix)) %>% 
  arrange(desc(discrims))
to_fix$gov <- readRDS('keep_cols_gov.rds')

#to_fix$gov <- NULL
vote_matrix <- prepare_matrix(cleaned=cleaned,legislature=legislature,to_fix_type=identify,
                              to_fix=to_fix,
                              to_pin_bills=c('no_gov','no_opp'),
                              split_absences=split_absences,absent_bill=readRDS('keep_cols_abs.rds'),
                              to_run=to_run,use_nas=use_nas)

if(identify=='ref_discrim') {
  opp_num <- vote_matrix$opp_num
  gov_num <- vote_matrix$gov_num
  vote_matrix <- vote_matrix$votes
}

# Number of legislators/bills in model
num_legis <- nrow(vote_matrix)
num_bills <- ncol(vote_matrix)
legislator_points <- rep(1:num_legis,times=num_bills)
bill_points <- rep(1:num_bills,each=num_legis)

# Need average participation by legislator

participation <- cleaned[[legislature]] %>% gather(bill,vote,matches('Bill')) %>% group_by(legis.names) %>% 
  summarize(particip_rate=1 - (sum(vote==4)/length(vote))) %>% mutate(particip_rate=scale(particip_rate))



#What to fix final bill at



Y <- c(vote_matrix)
Y[Y>3] <- 4

#Remove NAs

remove_nas <- !is.na(Y)
Y <- Y[remove_nas]
legislator_points <- legislator_points[remove_nas]
bill_points <- bill_points[remove_nas]

script_file <- if(to_run<3) { 
  "R_Scripts/binary_discrimfix.stan" } else {
    if(split_absences==TRUE) {
      'R_Scripts/ordinal_split_absence.stan'
    } else if(identify=='edstan'){
    "R_Scripts/nominate_ordinal.stan"
    } else if(identify=='ref_bills'){
      'R_Scripts/ordinal_billfix.stan'
    } else if(identify=='ref_discrim') {
      'R_Scripts/ordinal_discrimfix.stan'
    }
  }
model_code <- readChar(script_file,file.info(script_file)$size)
 
#Parameter of inference is L_open (legislator points) and B_adj (bill points). 
#Identification happens by assigning the last bill to be a reference category for the other bills
# And using an informative normal prior on the legislator points
# Best fit so far for 3-choice ordinal: N(0,5) on ideal points, sigma ~ N(0,5),N(1,.1) on cutpoints
source('R_Scripts/vb_starts.R')
starts <- readRDS('vb_starts.rds')
make_starts <- function() {
  starts
}
if(use_vb==TRUE) {
  compiled_model <- stan_model(model_code=model_code,model_name="Nominate: 1 dimension")
  sample_fit <- vb(object=compiled_model,data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                                                     bb=bill_points,fixed_bills=if(identify=='ref_bills') {
                                                       length(to_fix$final_constraint)
                                                     } else if(identify=='ref_discrim') {
                                                       opp_num+gov_num
                                                     },bill_pos=to_fix$constraint_num),
                   algorithm='meanfield')
} else if(categorical==TRUE) {
  compiled_model <- stan_model(file=model_code,model_name="Nominate: 1 dimension")
  sample_fit <- sampling(compiled_model,data = list(y=(Y-1), N=length(Y), J=num_legis, I=num_bills, jj=legislator_points,
                                                    ii=bill_points,
                                                    fixed_bills=length(to_fix$final_constraint),
                                                    opp_num=opp_num,
                                                    gov_num=gov_num,
                                                    bill_pos=to_fix$constraint_num),
                         init=0,iter=2000,chains=2,cores=2)
  
} else {
  compiled_model <- stan_model(model_code=model_code,model_name="Nominate: 1 dimension")

sample_fit <- sampling(compiled_model,data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                                              bb=bill_points,
                                              fixed_bills=if(identify=='ref_bills') {
                                                length(to_fix$final_constraint)
                                              } else if(identify=='ref_discrim') {
                                                opp_num+gov_num
                                              },
                                              bill_pos=to_fix$constraint_num,
                       opp_num=opp_num,gov_num=gov_num,particip=participation$particip_rate),
                       init=make_starts,
                       iter=2000,chains=4,cores=4,thin=4)


}
saveRDS(sample_fit,'most_recent_fit.rds')
Sys.setenv("plotly_username" = "bobkubinec")
Sys.setenv("plotly_api_key" = "8q00qm53km")

png(filename='split_absence_final.png',width=4000,height=4000,res=200)
plot_IRT(cleaned=cleaned,stan_obj=sample_fit,legislature="arp_votes",plot_param='L_open',ggplot=TRUE)
dev.off()




check_matrix <- as_data_frame(vote_matrix)
check_matrix$party_id <- cleaned[[legislature]]$bloc
colnames(vote_matrix)[1308]
xtabs(~Bill_2020 + party_id,data=check_matrix)

posterior <- rstan::extract(sample_fit,inc_warmup=FALSE,permuted=FALSE)
mcmc_trace(posterior,pars="avg_particip")



#saveToLocalRepo(summary(sample_fit),'data/',userTags=c('empirical','ordinal split absence constrain ZIP','ref_discrim','no parentheses'))





# alphas <- check_summary %>% as_data_frame %>% mutate(params=row.names(check_summary)) %>% 
#   filter(grepl('L_open',x = params))
# 
# data_frame(sigma=sigmas$mean,beta=betas$mean) %>% ggplot(aes(x=beta,y=sigma)) + geom_point(alpha=0.5) + theme_minimal() + 
#   stat_smooth(method = 'lm')
# 
# data_frame(beta2=betas2$mean,beta=betas$mean) %>% ggplot(aes(x=beta,y=beta2)) + geom_point(alpha=0.5) + theme_minimal() + 
#   stat_smooth(method = 'lm')

#Keep only 1 chain

lookat_params <- rstan::extract(sample_fit,permuted=FALSE)
lookat_params <- lookat_params[,2,]
sigmas_est <- lookat_params[,grepl('sigma_adj',colnames(lookat_params))]
sigmas2_est <- lookat_params[,grepl('sigma_abs\\[',colnames(lookat_params))]

chisqs <- apply(vote_matrix,2,function(x) {
  x <- table(x)
  chisq <- chisq.test(x)
  return(chisq$statistic)
})

data_frame(sigma_est=sigmas2_est$avg,chisq=chisqs) %>% ggplot(aes(x=sigma_est,y=chisqs)) + geom_point(alpha=0.5) + theme_minimal() + 
  stat_smooth(method = 'lm')
colnames(vote_matrix)[1280]

xtabs(~Bill_3897+ party_id,data=check_matrix)

# sigmas2_est <- arrange(sigmas2_est,avg)
# keep_cols <- as.numeric(stringr::str_extract(sigmas2_est$param_name,'[0-9]+')[1:20])
# keep_cols <- colnames(vote_matrix)[keep_cols]
# saveRDS(keep_cols,'keep_cols_abs.rds')
# 
# sigmas <- arrange(sigmas,mean)
# keep_cols <- as.numeric(stringr::str_extract(sigmas$params,'[0-9]+')[1:60])
# keep_cols <- colnames(vote_matrix)[keep_cols]
# saveRDS(keep_cols,'keep_cols_gov.rds')

source('R_Scripts/Generate_posterior.R')
