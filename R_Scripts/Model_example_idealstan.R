# New file to run models using idealstan package

require(dplyr)
require(magrittr)
require(tidyr)
require(idealstan)

#' Function to change names
.change_names <- function(x) {
  numeric_x <- as.numeric(x)
  test_num <- is.na(numeric_x)
  out_names <- c(x[test_num],paste0("Bill_",x[!test_num]))
  return(out_names)
}

# Load in new data and also old data on ARP (as new ARP data is not complete with bills + articles)
arp_votes <- data.table::fread("data/ARP_votes_all.csv",
                               header=TRUE) %>% as_data_frame
names(arp_votes) <- .change_names(names(arp_votes))

anc_votes <- data.table::fread('data/ANC_votes.csv',
                               sep=',',header=TRUE,strip.white=TRUE) %>% as_data_frame
names(anc_votes) <- .change_names(names(anc_votes))



# Load ARP bill names and labels, change bill names
arp_vote_names <- data.table::fread('data/ARP_votes_names.csv') %>% as_data_frame

# Load ANC bill names and labels
anc_vote_names <- data.table::fread('data/ANC_votes_labels.csv',
                                    col.names = c('bill.names','bill.id')) %>% as_data_frame

# Load member demographics for ANC and ARP
arp_members <- data.table::fread('data/members_ARP.csv',
                                 col.names=c('id','legis_names','sex','dob','pob','country','job','jobcat',
                                             'elec_list','parliament_bloc')) %>% as_data_frame

anc_members <- data.table::fread('data/members_ANC.csv',
                                 col.names=c('id','legis_names','sex','dob','pob','country','job',
                                             'elec_list','parliament_bloc')) %>% as_data_frame

# Attempt a match on ARP/ANC members (fuzzy join)

match_keys <- sapply(anc_members$legis_names,function(x) stringdist::amatch(x=x,table=arp_members$legis_names,maxDist=2))
to_update <- arp_members$legis_names[match_keys]
# use ARP spelling for any overllapping legislators

anc_members <- anc_members %>% mutate(legis_names=ifelse(is.na(match_keys),legis_names,to_update))

# Drop empty columns in the vote data

anc_votes <- anc_votes %>% select_if(function(x) {
  count_nas <- sum(is.na(x))
  if(count_nas==length(x)) {
    FALSE
  } else {
    TRUE
  }
})

arp_votes <- arp_votes %>% select_if(function(x) {
  count_nas <- sum(is.na(x))
  if(count_nas==length(x)) {
    FALSE
  } else {
    TRUE
  }
})

anc_votes$type <- 'ANC'
arp_votes$type <- 'ARP'
anc_members$type <- 'ANC'
arp_members$type <- 'ARP'


#  Now run a model with just the ANC data
# Need to pass in the vote matrix and legislator data separately

vote_matrix <- select(anc_votes,matches('Bill')) %>% as.matrix
ideal_data <- make_idealdata(vote_data=vote_matrix,legis_data=select(arp_votes,-matches('Bill')),
                            votes=c('contre','abstenu','pour'),abs_vote='')

ideal_model <- estimate_ideal(idealdata=ideal_data,nchains=1,niter=100,warmup=20)
