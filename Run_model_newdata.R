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
subset_party <- c("Bloc Al Horra","Mouvement Nidaa Tounes")
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

to_fix  <- fix_bills(legislator=legislator,party=c("Bloc Al Horra","Mouvement Nidaa Tounes"),
                     vote_data=cleaned,legislature="arp_votes")

# Prepare matrix for model 

vote_matrix <- prepare_matrix(cleaned=cleaned,legislature="arp_votes",to_fix=to_fix)

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


compiled_model <- stan_model(model_code=model_code,model_name="Nominate: 1 dimension")
if(use_vb==TRUE) {
sample_fit <- vb(object=compiled_model,data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                                                   bb=bill_points,fixed_bills=length(constraint_num),bill_pos=constraint_num),algorithm='meanfield')
} else {
sample_fit <- sampling(compiled_model,data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                                              bb=bill_points,fixed_bills=length(bill_pos),bill_pos=bill_pos,cut_breaks=cut_breaks),
                       init=0,iter=1000,chains=2,cores=2)
}
means_fit <- summary(sample_fit)[[1]]
legis_means <- as.data.table(means_fit[grepl("L_open\\[",row.names(means_fit)),])
legis_means$vote_match <- row.names(all_matrices[[to_run]])
legis_means <- merge(legis_means,combined_members,by.x='vote_match',by.y='vote_match',all.x = TRUE)

  # Plot Stan version points as a summary
legis_means <- legis_means[order(mean),]

ggplot(legis_means,aes(y=reorder(legis_names,mean),x=mean,colour=parliament_bloc)) + geom_point() + my_theme +
  geom_text(aes(label=legis_names),check_overlap = TRUE,hjust=2) + facet_wrap(~type) +
  geom_vline(xintercept=0) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  geom_errorbarh(aes(xmin=`2.5%`,xmax=`97.5%`)) + ylab("") + xlab("Political Position (Right versus Left)") 

ggsave("output_graphs/Combined_ARP_ANC.pdf",width=20,height=15,units="in")

# Combined without facet

ggplot(legis_means,aes(y=reorder(legis_names,mean),x=mean,colour=parliament_bloc)) + geom_point() + my_theme +
  geom_text(aes(label=reorder(legis_names,mean)),check_overlap = TRUE,hjust=2) + 
  geom_vline(xintercept=0) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  geom_errorbarh(aes(xmin=`2.5%`,xmax=`97.5%`)) + ylab("") + xlab("Political Position (Right versus Left)") 

ggsave("output_graphs/ARP_ANC_all.pdf",width=20,height=15,units="in")
