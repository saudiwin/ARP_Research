# Run Ideal point models on Tunisian ARP data from Bawsala
# Robert Kubinec 6-4-2016

# Read in data
require(pscl)
require(car)
require(emIRT)
require(reshape2)
require(anominate)
require(stringr)
require(data.table)
require(rstan)
source("Ggplot2_theme.R")
source('emIRT_graph.R')

raw_data2 <- readRDS('data/raw_data2.rds')
raw_data <- readRDS('data/raw_data.rds')

# Recode the party variable
raw_data2$Party <- factor(raw_data2$Party,levels=c("Afek Tounes, le mouvement national et l'appel des tunisiens à l'étranger",
                                 "Bloc Al Horra",
                                 "Bloc Social-Démocrate",
                                 "Front Populaire",
                                 "Mouvement Ennahdha",
                                 "Mouvement Nidaa Tounes",
                                 "Union Patriotique Libre","Aucun bloc"),
       exclude=c("",NA),
       labels=c("Afek","AlHorra","Social-Dem","FP","Nahda","Nidaa","UPL","Independent"))

# Merge datasets to create two data frames, one with votes and the other with party/legislator data

info_legis <- data.table(raw_data)
setkey(info_legis,"legis.names")
party_info <- data.table(raw_data2[,1:2],key = "legis.names")
info_legis <- party_info[info_legis]
# Correct a type on the Bawsala data
info_legis <- info_legis[legis.names=='Said Aidi',Party:='Nidaa']

# This is all the votes without legislator data. To run emIRT with an ordinal DV, I have to code nay=1,abstain=2,
# yay=3,NA=0. I also create two binary vote matrices, one where yay=1,nay=-1, and one where abstain=1,not-abstain=-1

votes <- info_legis[,3:length(info_legis),with=FALSE]
votes_matrix <- as.matrix(data.frame(lapply(votes,function(x) as.numeric(factor(x,exclude="")))))
row.names(votes_matrix) <- info_legis$legis.names
# vote_names <- str_extract(names(votes),"N\\.[0-9][0-9]?[0-9]?\\.201[0-9]")
# vote_names <- ifelse(is.na(vote_names),names(votes),vote_names)


votes_matrix_bin <- lapply(votes,function(x) factor(x,levels = c(NA,"contre","pour"),exclude = "abstenu"))
votes_matrix_all <- lapply(votes,function(x) factor(x,levels = c(NA,"contre","abstenu","pour")))
votes_matrix_abst <- lapply(votes,function(x) factor(x,levels = c(NA,"abstenu","pour",'contre')))

votes_matrix_bin <- as.matrix(as.data.frame(lapply(votes_matrix_bin,function(x) as.numeric(x))))
votes_matrix_all <- as.matrix(as.data.frame(lapply(votes_matrix_all,function(x) as.numeric(x))))
votes_matrix_abst <- as.matrix(as.data.frame(lapply(votes_matrix_abst,function(x) as.numeric(x))))

# Recode matrices for emIRT
votes_matrix_bin <- apply(votes_matrix_bin,2,function(x) x <- recode(x,'3=1;2=-1;NA=0'))
votes_matrix_all <- apply(votes_matrix_all,2,function(x) x <- recode(x,'NA=0'))
votes_matrix_abst <- apply(votes_matrix_abst,2,function(x) x <- recode(x,'1=1;c(2,3)=-1;NA=0'))

# Make a list with all matrices. Add row names for legislators
# Filter matrices such that legislators with few votes and unanimous bills are dropped
all_matrices <- list(binary=votes_matrix_bin,ordinal=votes_matrix_all,abstain_bin=votes_matrix_abst)
bills_agree <- apply(votes_matrix,2,function(x) mean(!is.na(x)))
num_votes  <- apply(votes_matrix,1,function(x) length(x[!is.na(x)]))
names(num_votes) <- info_legis$legis.names


revise_data <- function(x) {
  row.names(x) <- info_legis$legis.names
  x <- x[(num_votes>(mean(num_votes)-2*sd(num_votes))),]
  x <- x[,((bills_agree>0.025) & (bills_agree<0.975) & !is.nan(bills_agree))]
  x
}
all_matrices <- lapply(all_matrices,revise_data)
info_legis <- info_legis[(num_votes>(mean(num_votes)-2*sd(num_votes))),]
# Number of legislators/bills in model

J <- nrow(all_matrices[[1]])
K <- ncol(all_matrices[[1]])

# Run binary emIRT (nay=-1, yay=-1, abstain/absent=NA)

bin_yaynay <- binIRT(.rc=list(votes=all_matrices$binary),.starts=getStarts(nrow(all_matrices$binary),
                                                            ncol(all_matrices$binary),
                                                            .D=1),
                                              .priors=makePriors(nrow(all_matrices$binary),
                                                                 ncol(all_matrices$binary),
                                                                 .D=1),
                     .anchor_subject = which(row.names(all_matrices$binary)=='Fathi Chamkhi'))

# Standard Errors
bin_yaynay <- boot_emIRT(bin_yaynay,Ntrials=100,.data = list(votes=all_matrices$binary),
                         .starts=getStarts(nrow(all_matrices$binary),
                          ncol(all_matrices$binary),.D=1),
                         .priors=makePriors(nrow(all_matrices$binary),
                                            ncol(all_matrices$binary),
                                            .D=1),
                         .control=list(threads=bin_yaynay$runtime$threads,thresh=bin_yaynay$runtime$tolerance,
                                       maxit=500,checkfreq=50,verbose=FALSE))

#All parties plot
plot(bin_yaynay, parties=info_legis$Party, legis.names=info_legis$legis.names)
#Subset of one party
plot(bin_yaynay, parties=info_legis$Party, legis.names=info_legis$legis.names,subset_name=c('Nidaa','AlHorra'))
plot(bin_yaynay, parties=info_legis$Party, legis.names=info_legis$legis.names,subset_name=c('Nahda','AlHorra'))



# Run binary emIRT (abstain=1,not abstain=-1,absent=0)

bin_abstain <- binIRT(.rc=list(votes=all_matrices$abstain_bin),.starts=getStarts(nrow(all_matrices$abstain_bin),
                                                                           ncol(all_matrices$abstain_bin),
                                                                           .D=1),
                     .priors=makePriors(nrow(all_matrices$abstain_bin),
                                        ncol(all_matrices$abstain_bin),
                                        .D=1),
                     .anchor_subject = which(row.names(all_matrices$abstain_bin)=='Fathi Chamkhi'))

bin_abstain <- boot_emIRT(bin_abstain,.data=list(votes=all_matrices$abstain_bin),.starts=getStarts(nrow(all_matrices$abstain_bin),
                                                                                 ncol(all_matrices$abstain_bin),
                                                                                 .D=1),
                      .priors=makePriors(nrow(all_matrices$abstain_bin),
                                         ncol(all_matrices$abstain_bin),
                                         .D=1),
                      .control=list(threads=bin_yaynay$runtime$threads,thresh=bin_yaynay$runtime$tolerance,
                                    maxit=500,checkfreq=50,verbose=FALSE))

#All parties plot
plot(bin_abstain, parties=info_legis$Party, legis.names=info_legis$legis.names)

#Subset of one party. Compare this one to the all vote data plot -- AlHorra tends to cluster together when 
#predicting absentions
plot(bin_abstain, parties=info_legis$Party, legis.names=info_legis$legis.names,subset_name=c('Nidaa','AlHorra'))
plot(bin_abstain, parties=info_legis$Party, legis.names=info_legis$legis.names,subset_name=c('Nahda','AlHorra'))

# Run ordinal emIRT (yes=1,abstain=2,no=3)
# Unfortunately, model does not work

# These are the constraints on the model. FP/Jebha Shabiya is constrained to negative on the first dimension,
# while NT is constrained to negative. Ideally we would constrain bills on the second dimension, but I still don't
# have a bill key, so I don't know which ones are which. To run the second dimension, I am currently constraining
# Afek Tounes to positive values

first_d_neg <- 'FP'
first_d_pos <- 'Nidaa'
second_d_pos <- 'Afek'
     
# Need to generate starting values for the algorithm

start.values <- list(alpha=matrix(rep(0,K),nrow=K,ncol=1),
                     beta=matrix(runif(K,-1,1),nrow=K,ncol=1),
                     x=matrix(runif(J,-4,4),nrow=J,ncol=1),
                     DD=matrix(rep(0.5,K),nrow=K,ncol=1),
                     tau=matrix(rep(-0.5,K),nrow=K,ncol=1))

priors <- vector(mode = "list")
priors$x <- list(mu = matrix(0,1,1), sigma = matrix(1,1,1) )
priors$beta <- list(mu = matrix(0,2,1), sigma = matrix(diag(25,2),2,2))

#Set constraints on the ideal point parameters by counting row indices by party
# constrain_pos_1d <- c(which(info_legis[row.names(all_matrices[[1]]),]$Party %in% first_d_pos))
# start.values$x[constrain_pos_1d] <- 1
row.names(all_matrices$ordinal) <- NULL
ord_all <- ordIRT(.rc=all_matrices$ordinal,.starts=start.values,
                  .priors=priors)



#Code to run a comparison with DW-NOMINATE
# vote_data <- rollcall(data=vote_data)
# 
# wnom_fit <- wnominate(vote_data,polarity=constrain_legis[1],dims=1)
# legis_data <- wnom_fit$legislators
# legis_data$stan_mean <- legis_means[,1]
# legis_data$lowci <- legis_means[,4]
# legis_data$highci <- legis_means[,8]
# legis_data$scale_mean <- scale(legis_means[,1])
# legis_data$id <- row.names(legis_data)
# 
# legis_melt <- melt(legis_data,measure.vars=c("coord1D","stan_mean"))
# 
# #Check correlation between estimates. It is generally correlating at 0.96
# 
# cor(legis_data$coord1D,legis_data$stan_mean)
# 
# # Plot W-nominate points against Stan version. Notable difference is that W-Nominate over-predicts variation in 
# # The democratic party, whereas the Stan version puts the Democrats all in the same box.
# 
# ggplot(legis_data,aes(x=coord1D,y=stan_mean)) + geom_point() +
#   geom_text(aes(label=id),check_overlap = TRUE) + geom_abline(intercept=0,slope=1) + my_theme +
#   geom_pointrange(aes(ymin=lowci,ymax=highci),alpha=0.5) + ylab("Stan Estimates") + xlab("WNOMINATE Estimates")
# ggsave("stan_v_nominate.pdf")
# 
# 
# # Plot Stan version points as a summary
# 
# ggplot(legis_data,aes(y=reorder(id,stan_mean),x=stan_mean)) + geom_point() + my_theme +
#   geom_text(aes(label=id),check_overlap=TRUE) + geom_vline(xintercept=0) + theme(axis.text.y=element_blank()) +
#   geom_errorbarh(aes(xmin=lowci,xmax=highci))
# 
# ggsave("stan_legis.pdf")
# 
