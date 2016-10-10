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
legislature <- "ARP"
# Use variational inference? Faster, but less accurate
use_vb <- FALSE
# Convert absences to a separate category in ordinal regression?
use_nas <- FALSE
# Which dataset to use? Put 1 for binary, 2 for abstain, 3 for ordinal
to_run <- 3
# Use only a sample of bills/legislators?
sample_it <- FALSE
# Legislator to use as a reference for IRT model
refleg <- "ARP_Bochra Belhaj Hamida"

# clean and load data for the kind of analysis (ordinal v. binary)




# Use Bouchra's votes to filter out those particular bills that are useful for pinning the scale

cleaned <- clean_data(keep_legis=keep_legis,use_subset=use_subset,subset_party=subset_party,
                    use_both=use_both,refleg=refleg,
                    legis=1,use_vb=use_vb,use_nas=use_nas,to_run=to_run,sample_it=sample_it)



output  <- fix_bills(legislator="Bochra Belhaj Hamida",party=c("Bloc Al Horra","Mouvement Nidaa Tounes"),
                     party_data=combined_members,vote_data=combined_ordinal,legislature="arp_votes")






# Now do more data processing 



num_votes <- lapply(all_matrices,apply,1,function(x) {
  y <- sum(!is.na(x))
})

bills_agree <- lapply(all_matrices,apply,2,function(x) {
  # Should be at least two types of votes per bill
  y <- if(length(table(x))<2) {
    FALSE
  } else {
    TRUE
  }
  y
})

revise_data <- function(x,to_keep) {
  to_change <- all_matrices[[x]]
  to_change <- to_change[,bills_agree[[x]]]
  to_change <- to_change[num_votes[[x]]>to_keep,]
  to_change
  }
all_matrices <- lapply(1:length(all_matrices),revise_data,to_keep=keep_legis)

if(use_both==FALSE) {
  all_matrices <- lapply(all_matrices,function(x) x[grepl(legislature,row.names(x)),])
}




# Number of legislators/bills in model
num_legis <- nrow(all_matrices[[to_run]])
num_bills <- ncol(all_matrices[[to_run]])
legislator_points <- rep(1:num_legis,times=num_bills)
bill_points <- rep(1:num_bills,each=num_legis)

# Create ideal point cutoffs
bill_ideal <- rnorm(1000,1)
legis_ideal <- rnorm(1000,1)
raw_scores <- dnorm(bill_ideal,legis_ideal,0.5,log=TRUE)
cuts <- quantile(raw_scores,probs = c(0.25,0.5,0.75))
cut_breaks <- cuts[2:3] - cuts[1:2]
#What to fix final bill at

#First, select only those bills with lowest X-sq scores (i.e., equal proportions among categories)

# x_sqs <- apply(all_matrices[[to_run]],2,function(x) {
#   if(length(unique(x[!is.na(x)])) > 2) {
#     value <-   table(x) %>% chisq.test()
#     return(value$statistic) } else {
#       return(NA)
#     }
#   
# })
# upper_bound <- quantile(x_sqs,probs=.05,na.rm=TRUE)
# move_obs <- which(x_sqs<upper_bound & !is.na(x_sqs))
# 
all_matrices[[to_run]] <- cbind2(all_matrices[[to_run]][,-final_constraint],all_matrices[[to_run]][,final_constraint])


# if(use_nas==TRUE) {
# bill_pos <- sapply(all_matrices[[to_run]][num_legis,(num_bills-1):num_bills],switch,
#                    -1,
#                    -0.5,
#                    0.5,
#                    1)
# } else {
#   bill_pos <- sapply(all_matrices[[to_run]][num_legis,(num_bills-1):num_bills],switch,
#                      -1,
#                      0,
#                      1)
# }
# bill_pos <- bill_pos[(!is.na(bill_pos) | !is.null(bill_pos))]

Y <- c(all_matrices[[to_run]])

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
