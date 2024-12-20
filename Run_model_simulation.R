# Model of Tunisian legislative ideal points
# Data provided by NGO Bawsala
# Analysis by Robert Kubinec and Sharan Grewal
# July 5th, 2016


require(rstan)
require(bawsala)

#CONTROL PANEL

# Check out partial credit IRT
categorical <- FALSE

# What type of identification to use
identify <- 'ref_discrim'

# Which of the legislatures to use-- ARP or ANC
use_both <- FALSE
legislature <- "arp_votes"
# Use variational inference? Faster, but less accurate
use_vb <- FALSE

to_run <- 3


# clean and load data for the kind of analysis (ordinal v. binary)




# Use Bouchra's votes to filter out those particular bills that are useful for pinning the scale

cleaned <- simulate_cleaned(legislature=legislature)
true_params <- cleaned$true_legis
cleaned <- cleaned$cleaned
# to_fix  <- fix_bills(legislator=legislator,party=subset_party,
#                      vote_data=cleaned,legislature=legislature)

to_fix <- fix_bills_discrim(opp='3',gov="1",
                            vote_data=cleaned,legislature=legislature)

# Prepare matrix for model 

vote_matrix <- prepare_matrix(cleaned=cleaned,legislature=legislature,to_fix_type=identify,
                              to_fix=to_fix,
                              to_pin_bills=c('no_gov','no_opp'))

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

#What to fix final bill at



Y <- c(vote_matrix)

#Remove NAs
remove_nas <- !is.na(Y)
Y <- Y[remove_nas]
legislator_points <- legislator_points[remove_nas]
bill_points <- bill_points[remove_nas]

script_file <- if(to_run<3) { 
  "R_Scripts/nominate_test_simple.h" } else {
    if(identify=='edstan') {
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
                       opp_num=opp_num,gov_num=gov_num),
                       iter=1000,chains=2,cores=2)


}



plot_IRT(cleaned=cleaned,stan_obj=sample_fit,legislature="arp_votes",plot_param='L_open',true_params=true_params)


require(dplyr)
require(bayesplot)
require(archivist)

saveToLocalRepo(sample_fit,'data/',userTags=c('simulation','ordinal','ref_discrim','constrain bills'))

check_matrix <- as_data_frame(vote_matrix)
check_matrix$party_id <- cleaned[[legislature]]$bloc
colnames(vote_matrix)[2]
xtabs(~Bill_2 + party_id,data=check_matrix)
