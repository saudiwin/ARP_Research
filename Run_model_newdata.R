# Model of Tunisian legislative ideal points
# Data provided by NGO Bawsala
# Analysis by Robert Kubinec and Sharan Grewal
# July 5th, 2016

require(data.table)
require(rstan)
require(stringdist)
require(wnominate)
require(pscl)
require(emIRT)
source('R_Scripts/Ggplot2_theme.R')
source('R_Scripts/emIRT_graph.R')

#CONTROL PANEL

# Keep legislators with have voted on at least this many bills
keep_legis <- 1
# Use only the parties in the subset_party variable?
use_subset <- TRUE
subset_party <- c('Al Horra','Nidaa')
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



# Load in new data and also old data on ARP (as new ARP data is not complete with bills + articles)


arp_votes <- fread("data/ARP_votes_all.csv",header=TRUE)
setkey(arp_votes,'legis.names')
# Load ARP bill names and labels
arp_votes_names <- fread('data/ARP_votes_names.csv')
setkey(arp_votes_names,'bill.id')
anc_votes <- fread('data/ANC_votes.csv',sep=',',header=TRUE,strip.white=TRUE)
setkey(anc_votes,'legis.names')
# Load ANC bill names and labels
anc_vote_names <- fread('data/ANC_votes_labels.csv',col.names = c('bill.names','bill.id'))
setkey(anc_vote_names,'bill.id')
# Load member demographics for ANC and ARP
arp_members <- fread("data/members_ARP.csv",col.names=c('id','legis_names','sex','dob','pob','country','job','jobcat',
                                                       'elec_list','parliament_bloc'))
setkey(arp_members,'legis_names')
anc_members <- fread("data/members_ANC.csv",col.names=c('id','legis_names','sex','dob','pob','country','job',
                                                      'elec_list','parliament_bloc'))
setkey(arp_members,'legis_names')

# Attempt a match on ARP/ANC members (fuzzy join)

match_keys <- sapply(anc_members$legis_names,function(x) amatch(x=x,table=arp_members$legis_names,maxDist=2))
to_update <- arp_members$legis_names[match_keys]
# use ARP spelling for any overllapping legislators

anc_members[,legis_names:=ifelse(is.na(match_keys),legis_names,to_update)]

match_keys <- sapply(anc_votes$legis.names,function(x) amatch(x=x,table=arp_members$legis_names,maxDist=2))
to_update <- arp_members$legis_names[match_keys]

anc_votes[,legis.names:=ifelse(is.na(match_keys),legis.names,to_update)]

# Now that we have fixed the legislator names, let's combine the datasets for members and votes by rows

anc_votes$type <- 'ANC'
arp_votes$type <- 'ARP'
anc_members$type <- 'ANC'
arp_members$type <- 'ARP'

combined_members <- rbind.data.frame(anc_members,arp_members,fill=TRUE)
members_ids <- unique(combined_members$legis_names)
members_ids_codes <- paste0("Member_",1:length(members_ids))
member_data <- data.table(legis_names=members_ids,codes=members_ids_codes)
combined_members <- merge(combined_members,member_data,by='legis_names',all.x=TRUE)

combined_members$vote_match <- paste0(combined_members$type,"_",combined_members$legis_names)

combined_votes <- rbind.data.frame(arp_votes,anc_votes,fill=TRUE)
combined_votes <- merge(combined_votes,member_data,by.x='legis.names',by.y='legis_names',all.x=TRUE)
combined_votes$vote_match <- paste0(combined_votes$type,"_",combined_votes$legis.names)
# Create three versions: binary yes/no, binary yes/no v. abstain, ordinal
# Ordinal is in emIRT format

# drop some empty columns in the data (no votes recorded)

is_logical <- names(combined_votes)[-which(sapply(combined_votes,is.logical))]
combined_votes_real <- combined_votes[,is_logical,with=FALSE]
combined_votes_real <- combined_votes_real[,4:ncol(combined_votes_real),with=FALSE]

combined_bin <- data.matrix(as.data.frame(lapply(combined_votes_real,factor,levels = c("contre","pour"),exclude = "abstenu"),
                                          row.names=combined_votes$vote_match))
combined_abstain <- data.matrix(as.data.frame(lapply(combined_votes_real,factor,levels = c("contre","pour",'abstenu')),
                                              row.names=combined_votes$vote_match))
combined_ordinal <- data.matrix(as.data.frame(lapply(combined_votes_real,factor,levels = c("contre",'abstenu',NA,'pour')),
                                              row.names=combined_votes$vote_match))
if(use_nas==TRUE) {
  combined_ordinal <- apply(combined_ordinal,2,function(x) {
    x[x==3] <- 4
    x[is.na(x)] <- 3
    x
  })
}


# Make binary matrics all 0 and 1
combined_bin <- combined_bin -1

combined_abstain <- apply(combined_abstain,2,function(x) {
  x[x==2] <- 1
  x[x==3] <- 2
  x
})
combined_abstain <- combined_abstain - 1

all_matrices <- list(combined_bin,combined_abstain,combined_ordinal)



if(use_subset==TRUE) {
  to_subset <- rowSums(sapply(subset_party,grepl,combined_members$parliament_bloc))
  to_subset <- combined_members$vote_match[as.logical(to_subset)]
  all_matrices <- lapply(all_matrices,function(x) x[to_subset,])
  
  # Reorder based on Bochra
  
  all_matrices <- lapply(all_matrices,function(x) {
    current_names <- row.names(x)
    current_pos <- which(current_names=='ARP_Bochra Belhaj Hamida')
    new_names <- c(current_names[-current_pos],current_names[current_pos])
    x <- x[new_names,]
    x
  })
}

# Now do more data processing 

if(sample_it==TRUE) {
  all_matrices <- lapply(all_matrices, function(x) {
    x <-    x[sample(nrow(x),size = 15),sample(ncol(x),size=150)]
    x
  })
}

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
if(use_nas==TRUE) {
bill_pos <- sapply(all_matrices[[to_run]][num_legis,(num_bills-1):num_bills],switch,
                   -1,
                   -0.5,
                   0.5,
                   1)
} else {
  bill_pos <- sapply(all_matrices[[to_run]][num_legis,(num_bills-1):num_bills],switch,
                     -1,
                     0,
                     1)
}
bill_pos <- bill_pos[(!is.na(bill_pos) | !is.null(bill_pos))]

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
                                                   bb=bill_points,fixed_bills=length(bill_pos),bill_pos=bill_pos),algorithm='meanfield')
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
