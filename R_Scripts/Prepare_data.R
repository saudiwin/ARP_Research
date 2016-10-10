#' Load all of the Bawsalah data and clean it
#' @param data_loc A 2-element list with locations of both ARP and ANC data
#' @param keep_legis Number of legislator votes required to keep them in the dataset
#' @param use_subset TRUE/FALSE Use only a subset of the dataset?
#' @param if use_subset==TRUE, a vector of party bloc names with which to subset the data
#' @param use_both TRUE/FALSE Should both the ANC and ARP datasets be combined?
#' @param legislature Which legislator to use if use_both==FALSE. Options are "ANC" or "ARP"
#' @param use_vb TRUE/FALSE Whether to use variational bayesian inference from the Rstan package. It is quicker but less accurate.
#' @param use_nas TRUE/FALSE For ordinal data, whether absences should be coded as a separate category (TRUE) or coded as NA (FALSE)
#' @param to_run Which of the datasets to use in the analysis. Put 1 for binary yes/no, 2 for binary yes/no v. abstain, 3 for ordinal 
#' @param sample_it Whether to use a sample of the dataset for analysis. Useful for testing models.
#' @export
load_data <- function(data_loc=NULL,keep_legis=1,use_subset=FALSE,subset_party=c('Al Horra','Nidaa'),
                      use_both=FALSE,
                      legislature="ARP",use_vb=FALSE,use_nas=FALSE,to_run=3,sample_it=FALSE) {
  
  # Make binary matrics all 0 and 1
  combined_bin <- combined_bin -1
  
  combined_abstain <- apply(combined_abstain,2,function(x) {
    x[x==2] <- 1
    x[x==3] <- 2
    x
  })
  combined_abstain <- combined_abstain - 1
  
  all_matrices <- list(combined_bin,combined_abstain,combined_ordinal)
}


#' Function to take a data set, a legislator name, a majority party, and find bills to fix positions
#' @export
fix_bills <- function(legislator=NULL,party=NULL,party_data=NULL,vote_data=NULL) {
  
  leg_votes <- vote_data[legislator,]
  
  party_leg <- party_data$vote_match[party_data$parliament_bloc==party]
  
  party_vote <- vote_data[party_leg,]
  
  # Need party votes and also ratios of within-party votes
  
  unan_party_votes <- apply(party_vote,2,function(x) {

    if(length(unique(x[!is.na(x)]))==1) {
      unique(x[!is.na(x)])
    } else {
      NA
    }
  })
  
  party_ratio <- apply(party_vote,2,function(x) {
    tables <- prop.table(table(x))
    # Need to check and see if there is only one vote for the party. If there was, sort will screw up the data.frame
    if(length(tables)==1) {
      tables <- tables %>% as.data.frame
    } else {
      tables <- tables %>% sort(decreasing=TRUE) %>% as.data.frame
    }
    
    return(tables)
  })
  
  # Use the ratios of party votes for each piece of legislation to determine in which votes the reference 
  # legislator did not vote with the majority party
  
  leg_resist <- sapply(1:length(party_ratio),function(x) {
 
    if((!is.na(leg_votes[x])) && (leg_votes[x]!=party_ratio[[x]][1,1])) {
      party_ratio[[x]][1,2] %>% as.numeric %>% return
    } else {
      return(0)
    }
  })
  
  abstain_leg <- which(leg_resist==max(leg_resist) & leg_votes==2)
  yes_leg <- which(leg_resist==max(leg_resist) & leg_votes==3)
  no_leg <- which((leg_resist==max(leg_resist)) & leg_votes==1)
  with_party_leg <- which(unan_party_votes==leg_votes & leg_votes==1)[1]
  # Final columns 
  
  final_constraint <- c(abstain_leg[1],yes_leg[1],no_leg[1],with_party_leg)
  constraint_num <- c(0.5,0,1,-1)
  
  return(list(constrain_bills=final_constraint,constrain_pos=constraint_num))
}