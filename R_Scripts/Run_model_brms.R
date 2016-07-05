# Run Ideal point models on Tunisian ARP data from Bawsala
# Robert Kubinec 6-4-2016

# Read in data
require(pscl)
require(car)
require(brms)
require(reshape2)
require(stringr)
require(data.table)
require(rstan)
source("R_Scripts/Ggplot2_theme.R")
source('R_Scripts/emIRT_graph.R')

raw_data2 <- readRDS('data/raw_data2.rds')
raw_data <- readRDS('data/raw_data.rds')

#Raw_data has a legislator that is not in the original dataset

raw_data <- raw_data[raw_data$legis.names!='Said Aidi',]

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
raw_data2 <- data.table(raw_data2,key='legis.names')

# Merge datasets to create two data frames, one with votes and the other with party/legislator data

info_legis <- data.table(raw_data)
setkey(info_legis,"legis.names")
party_info <- data.table(raw_data2[,1:2,with=FALSE],key = "legis.names")
info_legis <- party_info[info_legis]
# Correct a type on the Bawsala data
info_legis <- info_legis[legis.names=='Said Aidi',Party:='Nidaa']

# This is all the votes without legislator data. To run emIRT with an ordinal DV, I have to code nay=1,abstain=2,
# yay=3,NA=0. I also create two binary vote matrices, one where yay=1,nay=-1, and one where abstain=1,not-abstain=-1

votes <- info_legis[,3:length(info_legis),with=FALSE]
votes_matrix <- as.matrix(data.frame(lapply(votes,function(x) as.numeric(factor(x,exclude="")))))
# There are oddly enough duplicate columns in the vote matrix
votes_matrix <- votes_matrix[,!duplicated(votes_matrix,MARGIN=2)]
row.names(votes_matrix) <- info_legis$legis.names
# vote_names <- str_extract(names(votes),"N\\.[0-9][0-9]?[0-9]?\\.201[0-9]")
# vote_names <- ifelse(is.na(vote_names),names(votes),vote_names)


votes_matrix_bin <- lapply(votes,function(x) factor(x,levels = c(NA,"contre","pour"),exclude = "abstenu"))
votes_matrix_all <- lapply(votes,function(x) factor(x,levels = c(NA,"contre","abstenu","pour")))
votes_matrix_all2 <- lapply(raw_data2[,3:ncol(raw_data2),with=FALSE],function(x) factor(x,levels = c(NA,"contre","abstenu","pour")))
votes_matrix_abst <- lapply(votes,function(x) factor(x,levels = c(NA,"abstenu","pour",'contre')))

votes_matrix_bin <- as.matrix(as.data.frame(lapply(votes_matrix_bin,function(x) as.numeric(x))))
votes_matrix_all <- as.matrix(as.data.frame(lapply(votes_matrix_all,function(x) as.numeric(x))))
votes_matrix_all2 <- as.matrix(as.data.frame(lapply(votes_matrix_all2,function(x) as.numeric(x))))
votes_matrix_abst <- as.matrix(as.data.frame(lapply(votes_matrix_abst,function(x) as.numeric(x))))

# Recode matrices for emIRT
votes_matrix_bin <- apply(votes_matrix_bin,2,function(x) x <- recode(x,'3=1;2=0;NA=NA'))
votes_matrix_all <- apply(votes_matrix_all,2,function(x) x <- recode(x,'NA=0'))
votes_matrix_all2 <- apply(votes_matrix_all2,2,function(x) x <- recode(x,'NA=0'))
votes_matrix_abst <- apply(votes_matrix_abst,2,function(x) x <- recode(x,'1=1;c(2,3)=0;NA=NA'))

checkcols <- function(x) {
  this_col <- x
  all_cols <- apply(votes_matrix_all2,2,identical,x)
  which(all_cols==TRUE)
}

match_bills <- as.numeric(apply(votes_matrix_all,2,checkcols))

bills_names <- colnames(raw_data2[,3:length(raw_data2),with=FALSE])[match_bills]
indices <- which(is.na(bills_names))
amend_names <- paste0("Amendment_",1:length(indices))
bills_names[indices] <- amend_names
colnames(votes_matrix_all) <- bills_names
# Make a list with all matrices. Add row names for legislators
# Filter matrices such that legislators with few votes and unanimous bills are dropped
all_matrices <- list(binary=votes_matrix_bin,ordinal=votes_matrix_all,abstain_bin=votes_matrix_abst)
bills_agree <- apply(votes_matrix_bin,2,function(x) mean(x,na.rm=TRUE))
na_vals <- apply(votes_matrix_bin,2,function(x) sum(is.na(x)))
num_votes  <- apply(votes_matrix,1,function(x) length(x[!is.na(x)]))
names(num_votes) <- info_legis$legis.names


revise_data <- function(x) {
  row.names(x) <- info_legis$legis.names
  x <- x[(num_votes>(mean(num_votes)-2*sd(num_votes))),]
  x <- x[,((bills_agree>0.01) & (bills_agree<0.99) & (na_vals<210))]
  x
}
all_matrices <- lapply(all_matrices,revise_data)
info_legis <- info_legis[(num_votes>(mean(num_votes)-2*sd(num_votes))),]
# Number of legislators/bills in model

J <- nrow(all_matrices[[1]])
K <- ncol(all_matrices[[1]])

# Create data for brms package 

votes_matrix_data <- as.data.frame(votes_matrix_bin)
votes_matrix_data$nawab <- row.names(votes_matrix_data)
votes_matrix_data <- reshape2::melt(votes_matrix_data,id="nawab",na.rm=TRUE)

#model1 <- brm(formula = value ~ 0 + (main + spec):variable + (0 + main|nawab),data = votes_matrix_data[1:1000,],family=bernoulli(type='2PL'))


# Compare with ed_stan

# Load packages
library(rstan)
library(edstan)

# Make the data list
data_dich <- irt_data(votes_matrix_bin)

# Fit the Rasch model
# fit_rasch <- irt_stan(data_dich, model = "2pl_latent_reg.stan",
#                       iter = 500, chains = 2,cores=2)

# View summary of parameter posteriors                    
# print_irt_stan(fit_rasch, data_dich)

#Compare with my nominate version
constrain_pos <- c(which(info_legis$Party=='Afek'))
# constrain_pos <- c(which(grepl('SESSIONS',row.names(sv.yay))),
#                   which(grepl('LEE',row.names(sv.yay))))
#                    which(grepl('INHOFE',row.names(sv.yay))),
#                    which(grepl('RISCH',row.names(sv.yay))))
#                    which(grepl('CRAPO',row.names(sv.yay))))
constrain_neg <- NULL
#constrain_neg <- c(which(grepl('WARREN',row.names(sv.yay))),
#                    which(grepl('REID',row.names(sv.yay))))
#                  which(grepl('WYDEN',row.names(sv.yay))),
#                    which(grepl('SCHATZ',row.names(sv.yay))))
#                    which(grepl('MENENDEZ',row.names(sv.yay))))

num_con_pos <- ifelse(is.null(constrain_pos),1,length(constrain_pos))
num_con_neg <- ifelse(is.null(constrain_neg),1,length(constrain_neg))
true_con_pos <- ifelse(is.null(constrain_pos),0,length(constrain_pos))
true_con_neg <- ifelse(is.null(constrain_neg),0,length(constrain_neg))
constrain_legis <- c(constrain_pos,constrain_neg)

# Reorder points which to use for constraining so that the constraints are always first.
# use just AlHorra & Nidaa
party <- info_legis$Party
info_legis <- info_legis[info_legis$Party %in% c('AlHorra','Nidaa'),]

all_matrices[[3]] <- all_matrices[[3]][party %in% c('AlHorra','Nidaa'),]
num_legis <- nrow(all_matrices[[3]])
num_bills <- ncol(all_matrices[[3]])
legislator_points <- rep(1:num_legis,times=num_bills)
bill_points <- rep(1:num_bills,each=num_legis)

all_legis <- 1:nrow(all_matrices[[3]])
not_legis <- all_legis[-constrain_legis]
new_legis <- c(constrain_legis,not_legis)
#all_matrices[[3]] <- all_matrices[[3]][new_legis,]
#info_legis <- info_legis[new_legis,]

# Create dummies, drop first category
#reorder
party_names <- as.character(unique(party))
party_names <- c('FP',party_names[-c(which(party_names=='FP'),which(party_names=='Nidaa'))])
party_dum <- sapply(unique(party),function(x) as.numeric(party==x))
# add intercept at the end because the first column is positive-constrained
party_dum <- cbind(party_dum,1)
party_init <- ifelse(party=='Afek',-0.5,0.5)
num_parties <- ncol(party_dum)


Y <- c(all_matrices[[3]])

#Remove NAs
remove_nas <- !is.na(Y)
Y <- Y[remove_nas]
legislator_points <- legislator_points[remove_nas]
bill_points <- bill_points[remove_nas]

script_file <- "R_Scripts/nominate_test.h"
model_code <- readChar(script_file,file.info(script_file)$size)

#Parameter of inference is L_adj (legislator points) and B_adj (bill points). The legislature and bill ideal points have been adjusted by 
#Subtracting the mean of the vector of legislature ideal points. This is recommended by Gelman (2005) to speed 
#up convergence, and indeed it does so. 

compiled_model <- stan_model(model_code=model_code,model_name="Nominate: 1 dimension")
#create some starting values for sigma to speed convergence

init_list <- lapply(1:2,function(x) list(sigma=rep(0.5,times=num_bills),L_open=party_init,B_yes=rep(0,times=num_bills)))

sample_fit <- vb(object=compiled_model,data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                                                   bb=bill_points,num_con_pos=num_con_pos,num_con_neg=num_con_neg,true_con_pos=0,
                                                   party_dum=party_dum,num_parties=num_parties,length_parties=nrow(party_dum),
                                                   true_con_neg=0),algorithm='meanfield',
                 eta=1,adapt_engaged=FALSE)

means_fit <- summary(sample_fit)[[1]]
legis_means <- means_fit[grepl("L_open\\[",row.names(means_fit)),]
# multiply scale by 10 because estimating sigmas separately throws off scale

#legis_means <- legis_means * 10

#Test with wnominate
require(wnominate)
vote_data <- list(votes=all_matrices[[3]],legis.names=row.names(all_matrices[[3]]))
vote_data <- rollcall(data=vote_data)

wnom_fit <- wnominate(vote_data,polarity=1,dims=1,minvotes=20)

legis_data <- wnom_fit$legislators
legis_data$stan_mean <- legis_means[,"mean"]
legis_data$lowci <- legis_means[,"2.5%"]
legis_data$highci <- legis_means[,"97.5%"]
legis_data$scale_mean <- scale(legis_means[,"mean"])
legis_data$id <- row.names(all_matrices[[3]])
legis_data$party <- info_legis$Party
updown <- legis_data$stan_mean>0
legis_data$nameup <- legis_data$id
legis_data$namedown <- legis_data$id
legis_data$nameup[!updown] <- NA
legis_data$namedown[updown] <- NA
legis_melt <- melt(legis_data,measure.vars=c("coord1D","stan_mean"))

#Check correlation between estimates. It is generally correlating at 0.96

cor(legis_data$coord1D,legis_data$stan_mean)

# Plot W-nominate points against Stan version. Notable difference is that W-Nominate over-predicts variation in 
# The democratic party, whereas the Stan version puts the Democrats all in the same box.



# Plot party intercepts as well

party_data <- as.data.table(summary(sample_fit,pars="delta")[[1]])
party_data <- party_data[-nrow(party_data),]
party_data$party <- c(party_names,"Intercept")
party_data$party_mean <- party_data$mean
party_data <- party_data[,c('party','party_mean'),with=FALSE]
setkey(party_data,'party')
legis_data <- data.table(legis_data,key='party')
legis_data_party <- party_data[legis_data]
ggplot(legis_data,aes(x=coord1D,y=stan_mean,colour=party)) + geom_point() +
  geom_text(aes(label=nameup),check_overlap = TRUE,hjust=2) + geom_text(aes(label=namedown),check_overlap = TRUE,hjust=-1) +
  geom_abline(intercept=0,slope=1) + my_theme +
  geom_pointrange(aes(ymin=lowci,ymax=highci),alpha=0.5) + ylab("Stan Estimates") + xlab("WNOMINATE Estimates") 
ggsave("stan_v_nominate.pdf")

# Plot Stan version points as a summary

ggplot(legis_data_party,aes(y=reorder(id,stan_mean),x=stan_mean,colour=party)) + geom_point() + my_theme +
  geom_text(aes(label=nameup),check_overlap = TRUE,hjust=2) + geom_text(aes(label=namedown),check_overlap = TRUE,hjust=-1) + 
  geom_vline(xintercept=0) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  geom_errorbarh(aes(xmin=lowci,xmax=highci)) + ylab("") + xlab("Political Position (Right versus Left)") 

ggsave("stan_legis.pdf",width=20,height=15,units="in")

means_fit <- summary(sample_fit)[[1]]
means_matrix <- means_fit[grepl("L_open\\[|L_party\\[",row.names(means_fit)),]
legis_means <- as.data.table(means_matrix)
legis_means$id <- str_extract(string = row.names(means_matrix),pattern = "[0-9]+")
legis_means$vartype <- gsub(pattern = "\\[.*\\]",replacement = "",x=row.names(means_matrix))

legis_data$stan_mean_1 <- abs(legis_means[vartype=='L_open',mean])
legis_data$lowci_1 <- abs(legis_means[vartype=='L_open',`2.5%`])
legis_data$highci_1 <- abs(legis_means[vartype=='L_open',`97.5%`])
legis_data$scale_mean_1 <- abs(scale(legis_means[vartype=='L_party',`mean`]))
legis_data$stan_mean_2 <- abs(legis_means[vartype=='L_party',`mean`])
legis_data$lowci_2 <- abs(legis_means[vartype=='L_party',`2.5%`])
legis_data$highci_2 <- abs(legis_means[vartype=='L_party',`97.5%`])
legis_data$scale_mean_2 <- abs(scale(legis_means[vartype=='L_party',`mean`]))

legis_data$id <- row.names(all_matrices[[1]])
legis_data$party <- info_legis$Party
updown <- legis_data$stan_mean_1>0
legis_data$nameup <- legis_data$id
legis_data$namedown <- legis_data$id
legis_data$nameup[!updown] <- NA
legis_data$namedown[updown] <- NA
legis_data$stan_mean_keep <- legis_data$stan_mean_1
legis_melt <- melt(legis_data,measure.vars=c("stan_mean_1","stan_mean_2"))

ggplot(legis_melt,aes(x=value,y=reorder(id,stan_mean_keep),colour=variable)) + geom_point() +
  geom_text(aes(label=party),check_overlap = TRUE,hjust=2) + 
  geom_abline(intercept=0,slope=1) + my_theme +
  geom_errorbarh(aes(xmin=lowci_1,xmax=highci_1),alpha=0.5) +  xlab("Ideal Point")


ggplot(legis_data,aes(x=stan_mean_1,y=reorder(id,stan_mean_1),colour=party)) + geom_point() +
  geom_text(aes(label=nameup),check_overlap = TRUE,hjust=2) + geom_text(aes(label=namedown),check_overlap = TRUE,hjust=-1) +
  geom_abline(intercept=0,slope=1) + my_theme +
  geom_errorbarh(aes(xmin=lowci_1,xmax=highci_1),alpha=0.5)
ggsave("stan_within_party.pdf",width=20,height=15,units="in")
ggplot(legis_data,aes(x=stan_mean_2,y=reorder(id,stan_mean_2),colour=party)) + geom_point() +
  geom_text(aes(label=nameup),check_overlap = TRUE,hjust=2) + geom_text(aes(label=namedown),check_overlap = TRUE,hjust=-1) +
  geom_abline(intercept=0,slope=1) + my_theme +
  geom_errorbarh(aes(xmin=lowci_2,xmax=highci_2),alpha=0.5)
ggsave("stan_between_party.pdf",width=20,height=15,units="in")
ggplot(legis_data,aes(x=stan_mean_2,y=stan_mean_1,colour=party)) + geom_point() +
  geom_text(aes(label=nameup),check_overlap = TRUE,hjust=2) + geom_text(aes(label=namedown),check_overlap = TRUE,hjust=-1) +
  geom_abline(intercept=0,slope=-1) + my_theme +
  geom_errorbarh(aes(xmin=lowci_2,xmax=highci_2),alpha=0.5)


# What we need to do is subtract the two columns to get within-party variation, will require some work

# within_party <- extract(sample_fit,pars=c("L_open","L_party"))
# # Identification issue in the party vs. individual effects
# combined_party <- (within_party$L_party - mean(within_party$L_party)) - (within_party$L_open - mean(within_party$L_party))
# combined_party_data <- data.table(stan_mean=apply(combined_party,2,mean),
#                                   stan_lowci=apply(combined_party,2,quantile,probs=.05 ),
#                                   stan_highci=apply(combined_party,2,quantile,probs=.95))
# combined_party_data$id <- legis_data$id
# combined_party_data$party <- legis_data$party
# ggplot(combined_party_data,aes(x=stan_mean,y=reorder(id,stan_mean),colour=party)) + geom_point() +
#   geom_text(aes(label=party),check_overlap = TRUE,hjust=2) +
#   geom_abline(intercept=0,slope=-1) + my_theme +
#   geom_errorbarh(aes(xmin=stan_lowci,xmax=stan_highci),alpha=0.5)

all_bills <- extract(sample_fit,pars=c("sigma"))[[1]]
all_bills_means <- apply(all_bills,2,mean)
bills_estimates <- data.table(bill_means=all_bills_means,id=bills_names[((bills_agree>0.01) & (bills_agree<0.99) & (na_vals<210))],
                              key='bill_means')
