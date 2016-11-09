# Model of Tunisian legislative ideal points
# Data provided by NGO Bawsala
# Analysis by Robert Kubinec and Sharan Grewal
# July 5th, 2016


require(rstan)
require(bawsala)
source('R_Scripts/Ggplot2_theme.R')
source('R_Scripts/emIRT_graph.R')

#CONTROL PANEL

# Keep legislators with have voted on at least this many bills
keep_legis <- 1
# Use only the parties in the subset_party variable?
use_subset <- TRUE
subset_party <- c("Bloc Al Horra","Mouvement Nidaa Tounes",'Front Populaire')
use_both <- FALSE
# Which of the legislatures to use-- ARP or ANC
legislature <- "arp_votes"
# Use variational inference? Faster, but less accurate
use_vb <- FALSE
# Convert absences to a separate category in ordinal regression?
use_nas <- FALSE
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

to_fix  <- fix_bills(legislator=legislator,party=subset_party,
                     vote_data=cleaned,legislature=legislature)

# Prepare matrix for model 

vote_matrix <- prepare_matrix(cleaned=cleaned,legislature=legislature,to_fix=to_fix)

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
    "R_Scripts/nominate_ordinal.h"
  }
model_code <- readChar(script_file,file.info(script_file)$size)

#Parameter of inference is L_open (legislator points) and B_adj (bill points). 
#Identification happens by assigning the last bill to be a reference category for the other bills
# And using an informative normal prior on the legislator points
# Best fit so far for 3-choice ordinal: N(0,5) on ideal points, sigma ~ N(0,5),N(1,.1) on cutpoints

compiled_model <- stan_model(model_code=model_code,model_name="Nominate: 1 dimension")
if(use_vb==TRUE) {
sample_fit <- vb(object=compiled_model,data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                                                   bb=bill_points,fixed_bills=length(to_fix$final_constraint),bill_pos=to_fix$constraint_num),algorithm='meanfield')
} else {
sample_fit <- sampling(compiled_model,data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                                              bb=bill_points,fixed_bills=length(to_fix$final_constraint),bill_pos=to_fix$constraint_num),
                       init=0,iter=1000,chains=4,cores=4)
}

plot_IRT(cleaned=cleaned,stan_obj=sample_fit,legislature="arp_votes")
