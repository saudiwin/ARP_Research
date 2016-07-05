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

#Vote filtering parameters: will change which legislators/bills are dropped

# Keep legislators with have voted on at least this many bills
keep_legis <- 10
use_subset <- TRUE
subset_party <- c('AlHorra','Nidaa')

# Put 1 for binary, 2 for abstain, 3 for ordinal
to_run <- 2

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
combined_ordinal <- data.matrix(as.data.frame(lapply(combined_votes_real,factor,levels = c("contre",'abstenu','pour')),
                                              row.names=combined_votes$vote_match))

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
}

# Now do more data processing 

num_votes <- lapply(all_matrices,apply,1,function(x) {
  y <- sum(!is.na(x))
})

bills_agree <- lapply(all_matrices,apply,2,function(x) {
  # Should be at least two types of votes per bill
  y <- if(length(table(x))==1) {
    FALSE
  } else {
    TRUE
  }
  y
})

revise_data <- function(x,to_keep) {
  to_change <- all_matrices[[x]]
  to_change <- to_change[num_votes[[x]]>to_keep,]
  to_change <- to_change[,bills_agree[[x]]]
  to_change
  }
all_matrices <- lapply(1:length(all_matrices),revise_data,to_keep=keep_legis)


# Number of legislators/bills in model
num_legis <- nrow(all_matrices[[to_run]])
num_bills <- ncol(all_matrices[[to_run]])
legislator_points <- rep(1:num_legis,times=num_bills)
bill_points <- rep(1:num_bills,each=num_legis)

Y <- c(all_matrices[[to_run]])

#Remove NAs
remove_nas <- !is.na(Y)
Y <- Y[remove_nas]
legislator_points <- legislator_points[remove_nas]
bill_points <- bill_points[remove_nas]

script_file <- "R_Scripts/nominate_test_simple.h"
model_code <- readChar(script_file,file.info(script_file)$size)

#Parameter of inference is L_open (legislator points) and B_adj (bill points). 
#Identification happens by assigning the last bill to be a reference category for the other bills
# And using an informative normal prior on the legislator points


compiled_model <- stan_model(model_code=model_code,model_name="Nominate: 1 dimension")

sample_fit <- vb(object=compiled_model,data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                                                   bb=bill_points),algorithm='meanfield')

means_fit <- summary(sample_fit)[[1]]
legis_means <- means_fit[grepl("L_std\\[",row.names(means_fit)),]


