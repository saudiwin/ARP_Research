# Run A-Nominate on Tunisian ARP data from Bawsala

# Read in data
require(pscl)
require(parallel)
require(stringr)
require(data.table)
require(rstan)
source("Ggplot2_theme.R")
first_d_constraint <- "Nidaa"
first_d_ref <- 'FP'
second_d_constraint <- "Afek"

#raw_data2 <- read.csv('arp_votes_v2.csv',header=TRUE,stringsAsFactors = FALSE)
#raw_data <- read.csv('arp_votes.csv',header=TRUE,stringsAsFactors = FALSE)
# keep_cols <- sapply(raw_data,is.character)
# raw_data <- raw_data[,keep_cols]
raw_data2 <- read.csv('arp_votes_coded.csv',header=TRUE,stringsAsFactors = FALSE)
raw_data <- readRDS('raw_data.rds')

# Recode the party variable
raw_data2$Party <- factor(raw_data2$Party,levels=c("Afek Tounes, le mouvement national et l'appel des tunisiens à l'étranger",
                                                   "Bloc Al Horra",
                                                   "Bloc Social-Démocrate",
                                                   "Front Populaire",
                                                   "Mouvement Ennahdha",
                                                   "Mouvement Nidaa Tounes",
                                                   "UPL","Aucun bloc"),
                          exclude=c("",NA),
                          labels=c("Afek","AlHorra","Social-Dem","FP","Nahda","Nidaa","UPL","Independent"))

info_legis <- data.table(raw_data)
setkey(info_legis,"legis.names")
party_info <- data.table(raw_data2[,1:3],key = "legis.names")
info_legis <- party_info[info_legis]
info_legis <- info_legis[legis.names=='Said Aidi',Party:='Mouvement Nidaa Tounes']
votes <- info_legis[,3:length(info_legis),with=FALSE]

vote_names <- str_extract(names(votes),"N\\.[0-9][0-9]?[0-9]?\\.201[0-9]")
vote_names <- ifelse(is.na(vote_names),names(votes),vote_names)
votes_matrix <- lapply(votes,function(x) factor(x,levels = c("contre","pour"),exclude = "abstenu"))
votes_matrix <- as.matrix(as.data.frame(lapply(votes_matrix,function(x) as.numeric(x) - 1)))
row.names(votes_matrix) <- info_legis$legis.names
num_votes  <- apply(votes_matrix,1,sum,na.rm=TRUE)
names(num_votes) <- row.names(votes_matrix)
votes_matrix <- votes_matrix[(num_votes>(mean(num_votes)-2*sd(num_votes))),]
# Remove bills that have near-unaninous agreement
bills_agree <- apply(votes_matrix,2,mean,na.rm=TRUE)
votes_matrix <- votes_matrix[,((bills_agree>0.025) & (bills_agree<0.975))]
# Remove columns with all missing data = coding error
num_missing <- apply(votes_matrix,2,function(x) length(x[is.na(x)]))
votes_matrix <- votes_matrix[,num_missing<216]

# put constraints into the model. For this model, one each for democrats and republicans



# New Constraints ---------------------------------------------------------

#Set constraints by counting row indices by party
# constrain_pos_1d <- c(which(info_legis[row.names(all_matrices[[1]]),]$Party %in% first_d_pos))
# constrain_neg_1d <- c(which(info_legis[row.names(all_matrices[[1]]),]$Party %in% first_d_neg))
# constrain_pos_2d <- c(which(info_legis[row.names(all_matrices[[1]]),]$Party %in% second_d_pos))
# 
# num_con_pos_1d <- ifelse(is.null(constrain_pos_1d),1,length(constrain_pos_1d))
# num_con_neg_1d <- ifelse(is.null(constrain_neg_1d),1,length(constrain_neg_1d))
# true_con_pos_1d <- ifelse(is.null(constrain_pos_1d),0,length(constrain_pos_1d))
# true_con_neg_1d <- ifelse(is.null(constrain_neg_1d),0,length(constrain_neg_1d))
# constrain_legis_1d <- c(constrain_pos_1d,constrain_neg_1d)
# 
# num_con_pos_2d <- ifelse(is.null(constrain_pos_2d),1,length(constrain_pos_2d))
# true_con_pos_2d <- ifelse(is.null(constrain_pos_2d),0,length(constrain_pos_2d))
# constrain_legis_2d <- c(constrain_pos_2d)

#constrain_legis <- sort(which(names_legis %in% first_d_constraint))
# all_legis <- 1:nrow(votes_matrix)
# not_legis <- all_legis[-constrain_legis]
# new_legis <- c(constrain_legis,not_legis)
# votes_matrix <- votes_matrix[new_legis,]
info_legis <- info_legis[(num_votes>(mean(num_votes)-2*sd(num_votes))),]
#info_legis <- info_legis[new_legis,]
party <- info_legis$Party
# Create dummies, drop first category
party_dum <- sapply(unique(as.character(party)),function(x) as.numeric(party==x))

without_cons <- colnames(party_dum)[-which(colnames(party_dum) %in% c(first_d_constraint,first_d_ref))]
new_cons <- c(first_d_ref,first_d_constraint,without_cons)
party_dum <- party_dum[,new_cons]
party_dum <- party_dum[,-1]
party_init <- ifelse(party==first_d_constraint,-0.5,0.5)
num_parties <- ncol(party_dum)
num_legis <- nrow(votes_matrix)
num_bills <- ncol(votes_matrix)
legislator_points <- rep(1:num_legis,times=num_bills)
bill_points <- rep(1:num_bills,each=num_legis)
Y <- c(votes_matrix)
na_filter <- !is.na(Y)
Y <- Y[na_filter]
legislator_points <- legislator_points[na_filter]
bill_points <- bill_points[na_filter]
fileName <- "nominate.h"
model_code <- readChar(fileName, file.info(fileName)$size)

compiled_model <- stan_model(model_code=model_code,model_name="Nominate: 1 dimension")

# Use variational inference because it is much faster but still quite accurate at ideal point estimation

sample_fit <- vb(object=compiled_model,data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                                                   bb=bill_points,party_dum=party_dum,num_parties=num_parties,
                                                   business=info_legis$Business))

# sample_fit <- sampling(object=compiled_model,
#                        data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
#                                    bb=bill_points,num_constraints=length(constrain_legis),constrain=constrain_legis),
#                        iter=500, chains=2,warmup=250,cores=2)

means_fit <- summary(sample_fit)[[1]]
legis_means <- means_fit[grepl("L_adj\\[",row.names(means_fit)),]

#Test with wnominate

vote_data <- list(votes=votes_matrix,legis.names=row.names(votes_matrix))
vote_data <- rollcall(data=vote_data)

wnom_fit <- wnominate(vote_data,polarity=1,dims=1)
legis_data <- wnom_fit$legislators
legis_data$stan_mean <- legis_means[,"mean"]
legis_data$lowci <- legis_means[,"2.5%"]
legis_data$highci <- legis_means[,"97.5%"]
legis_data$scale_mean <- scale(legis_means[,"mean"])
legis_data$id <- row.names(legis_data)
legis_data$party <- party
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

ggplot(legis_data,aes(x=coord1D,y=stan_mean,colour=party)) + geom_point() +
  geom_text(aes(label=nameup),check_overlap = TRUE,hjust=2) + geom_text(aes(label=namedown),check_overlap = TRUE,hjust=-1) +
  geom_abline(intercept=0,slope=1) + my_theme +
  geom_pointrange(aes(ymin=lowci,ymax=highci),alpha=0.5) + ylab("Stan Estimates") + xlab("WNOMINATE Estimates")
ggsave("stan_v_nominate.pdf")

# Plot Stan version points as a summary

ggplot(legis_data,aes(y=reorder(id,stan_mean),x=stan_mean,colour=party)) + geom_point() + my_theme +
  geom_text(aes(label=nameup),check_overlap = TRUE,hjust=2) + geom_text(aes(label=namedown),check_overlap = TRUE,hjust=-1) + 
  geom_vline(xintercept=0) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  geom_errorbarh(aes(xmin=lowci,xmax=highci)) + ylab("") + xlab("Political Position (Right versus Left)")

ggsave("stan_legis.pdf",width=20,height=15,units="in")

means_fit <- summary(sample_fit)[[1]]
bill_means <- data.frame(means_fit[grepl("B_adj\\[",row.names(means_fit)),])

bill_means$id <- paste0("Vote_",1:ncol(votes_matrix))
updown <- bill_means$mean>0
bill_means$nameup <- bill_means$id
bill_means$namedown <- bill_means$id
bill_means$nameup[!updown] <- NA
bill_means$namedown[updown] <- NA
# bill_means$type  <- dict$bill
# bill_means$type[bill_means$type=="None"] <- "Administrative/\nAmendment"
# bill_means$type[bill_means$type!="Administrative/\nAmendment"] <- "Legislation"
bill_means <- bill_means[bill_means$X2.5.>-2 & bill_means$X97.5.<2,]
ggplot(bill_means,aes(y=reorder(id,mean),x=mean)) + geom_point() + my_theme +
  geom_text(aes(label=nameup),check_overlap = TRUE,hjust=2) + geom_text(aes(label=namedown),check_overlap = TRUE,hjust=-1) + 
  geom_vline(xintercept=0) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  geom_errorbarh(aes(xmin=X2.5.,xmax=X97.5.)) + ylab("") + xlab("Political Position (Right versus Left)")

ggsave("stan_bills.pdf",width=30,height=18,units="in")

# Plot density of bill discrimination parameters
discrim_fit <- summary(sample_fit)[[1]]
legis_discrim <- discrim_fit[grepl("sigma\\[",row.names(discrim_fit)),]
plot(density(legis_discrim[,1]))

# look at interaction

samples_delta <- extract(sample_fit,pars=c('delta'))[[1]]
samples_bus <- extract(sample_fit,'business_eff')[[1]]
delta_means <- 1:ncol(samples_delta)
for(i in 1:ncol(samples_delta)) {
  delta_means[i] <- mean(samples_delta[i]) + mean(samples_bus)
}

# Remove bills that were dropped in estimation 

# dict$discrim_scores <- legis_discrim[,1]
# dict <- dict[order(dict$discrim_scores),]
# The discrimination parameter is working. Bills that have more narrow votes also have lower sigmas, while bills
# with higher votes have higher sigmas.


# pscl_data <- list(votes=votes_matrix,legis.names=info_legis$legis.names)
# 
# pscl_arp <- rollcall(data=pscl_data)
# 
# require(wnominate)
# 
# new_estimate1<- wnominate(pscl_arp,dims=1,polarity=which(info_legis$legis.names==first_d_constraint))
# new_estimate2 <- wnominate(pscl_arp,dims=2,polarity=c(which(info_legis$legis.names==first_d_constraint),
#                                                       which(info_legis$legis.names==second_d_constraint)))
# plot_data <- new_estimate1$legislators
# plot_data$partynames <- row.names(plot_data)
# plot_data_2 <- new_estimate2$legislators
# plot_data_2$partynames <- row.names(plot_data_2)
# ggplot(plot_data,aes(y=reorder(partynames,coord1D),x=coord1D)) + my_theme + geom_point() + geom_text(aes(label=partynames),check_overlap = TRUE,nudge_x=-.5) + theme(axis.text.y=element_blank())
# ggplot(plot_data_2,aes(y=coord2D,x=coord1D)) + my_theme + geom_point() + geom_text(aes(label=partynames),check_overlap = TRUE) + theme(axis.text.y=element_blank())
# 
# # Run the model in a-nominate, may take a few years
# 
# # Use Nahda as the 'right-leaning' legislators on Islamist-secular
# over_models <- function(x) {
#   if(x<6) {
# anominate_arp_oned <- anominate(pscl_arp,dims=1,polarity=which(info_legis$legis.names==first_d_constraint),verbose=TRUE,nsamp =50000,burnin = 5000,thin=10,
#       constrain=TRUE)
# return(anominate_arp_oned)
# }
# # Use Afek Tounes as the 'right-leaning' on economic dimension
# if(x>5) {
# anominate_arp_twod <- anominate(pscl_arp,dims=2,polarity=c(which(info_legis$legis.names==first_d_constraint),
#                                                            which(info_legis$legis.names==second_d_constraint)),
#                                 verbose=TRUE,nsamp=50000,thin=10,burnin=5000,constrain=TRUE)
# return(anominate_arp_twod)
# }
# }
# all_models <- mclapply(1:10,over_models,mc.cores=10)
# saveRDS(all_models,"All_anominate_models.rds")
# 
# get_legis  <- anominate_arp_oned$legislators[[1]]
# get_legis_quants <- data.frame(t(apply(get_legis,2,function(x) quantile(x = x,c(0.05,0.5,0.95)))))
# names(get_legis_quants) <- c("5_Perc","Median","95_Perc")
# get_legis_quants$mean <- apply(get_legis,2,mean)
# get_legis_quants$legis.names <- row.names(get_legis_quants)
# get_legis_quants <- data.table(get_legis_quants,key="legis.names")
# get_legis_quants <- get_legis_quants[info_legis]
# get_legis_quants$Party <- factor(info_legis$Party,levels=c("Afek Tounes, le mouvement national et l'appel des tunisiens à l'étranger",
#                                                                  "Bloc Al Horra",
#                                                                  "Bloc Social-Démocrate",
#                                                                  "Front Populaire",
#                                                                  "Mouvement Ennahdha",
#                                                                  "Mouvement Nidaa Tounes",
#                                                                  "Union Patriotique Libre","Aucun bloc"),
#                                                             exclude=c("",NA),
#                                  labels=c("Afek","AlHorra","Social-Dém","FP","Nahda","Nidaa","UPL","Independent"))
# ggplot(data=get_legis_quants,aes(y=reorder(legis.names,mean),x=mean,colour=Party)) + geom_point() + my_theme + 
#   geom_text(aes(label=legis.names),check_overlap=TRUE,hjust=1.5) + 
#    theme(axis.text.y=element_blank()) + ylab("") + xlab("Ideology: Islamist v. Secularist") +
#   geom_errorbarh(aes(xmax=`95_Perc`,xmin=`5_Perc`)) + geom_vline(xintercept=0,alpha=0.5) + scale_colour_brewer(palette="Set1")
# 
# get_legis_1  <- anominate_arp_twod$legislators[[1]]
# get_legis_2  <- anominate_arp_twod$legislators[[2]]
# get_legis_quants_1 <- data.frame(t(apply(get_legis_1,2,function(x) quantile(x = x,c(0.05,0.5,0.95)))))
# get_legis_quants_2 <- data.frame(t(apply(get_legis_2,2,function(x) quantile(x = x,c(0.05,0.5,0.95)))))
# names(get_legis_quants_1) <- c("5_Perc","Median","95_Perc")
# names(get_legis_quants_2) <- c("5_Perc","Median","95_Perc")
# get_legis_quants_1$mean <- apply(get_legis_1,2,mean)
# get_legis_quants_2$mean <- apply(get_legis_2,2,mean)
# get_legis_quants_1$SD <- apply(get_legis_1,2,sd)
# get_legis_quants_2$SD <- apply(get_legis_2,2,sd)
# get_legis_quants_1$legis.names <- factor(row.names(get_legis_quants_1))
# names(get_legis_quants_1) <- paste0("Oned",names(get_legis_quants_1))
# names(get_legis_quants_2) <- paste0("Twod",names(get_legis_quants_2))
# combined_legis <- cbind(get_legis_quants_1,get_legis_quants_2)
# combined_legis$Party2 <- factor(combined_legis$Party,levels=c("Afek Tounes, le mouvement national et l'appel des tunisiens à l'étranger",
#                                                                "Bloc Al Horra",
#                                                                "Front Populaire",
#                                                                "Mouvement Ennahdha",
#                                                                "Mouvement Nidaa Tounes",
#                                                                "Union Patriotique Libre"),
#                                exclude=c("",NA,"Aucun bloc","Bloc Social-Démocrate"),
#                                labels=c("Afek","AlHorra","FP","Nahda","Nidaa","UPL"))
# combined_legis$Party <- factor(combined_legis$Party,levels=c("Afek Tounes, le mouvement national et l'appel des tunisiens à l'étranger",
#                                                               "Bloc Al Horra",
#                                                               "Front Populaire",
#                                                               "Bloc Social-Démocrate",
#                                                               "Mouvement Ennahdha",
#                                                               "Mouvement Nidaa Tounes",
#                                                               "Union Patriotique Libre"),
#                                 exclude=c("",NA,"Aucun bloc"),
#                                 labels=c("Afek","AlHorra","FP","Social-Dém","Nahda","Nidaa","UPL"))
# 
# # Phenomenal code from stack exchange to plot cool ellipses
# 
# ellipseFun <- function(center = c(0, 0), axes = c(1, 1), npoints = 101){
#   tt <- seq(0,2*pi, length.out = npoints)
#   xx <- center[1] + axes[1] * cos(tt)
#   yy <- center[2] + axes[2] * sin(tt)
#   return(data.frame(x = xx, y = yy))
# }
# 
# ellipse_data <- data.frame()
# for(k in levels(factor(combined_legis$Onedlegis.names))){
#   ellipse_data <- rbind(ellipse_data, cbind(as.data.frame(with(combined_legis[combined_legis$Onedlegis.names == k,], ellipseFun(center = c(Twodmean, Onedmean), axes = c(TwodSD, OnedSD), npoints = 101))),Onedlegis.names = k))
# }
# ellipse_data <- data.table(ellipse_data,key="Onedlegis.names")
# ellipse_data <- ellipse_data[combined_legis]
# ggplot(data=ellipse_data,aes(y=Onedmean,x=Twodmean)) + geom_polygon(aes(x = x, y = y, group = Onedlegis.names,fill=Party), alpha = .5) + 
#   my_theme + 
#   geom_text(aes(label=Onedlegis.names),check_overlap=TRUE) +
#   ylab("Ideology: Islamist v. Secularist") + xlab("Economic Policy: Left vs. Right") +# Now for the ellipse code
#   coord_fixed() + scale_fill_brewer(palette="Set1") +
#   geom_hline(yintercept=0,alpha=0.5) + geom_vline(xintercept = 0,alpha=0.5)
# 
# ggplot(data=combined_legis,aes(y=Onedmean,x=Twodmean)) + geom_point() + my_theme + 
#   geom_text(aes(label=Onedlegis.names),check_overlap=TRUE,hjust=0.5,vjust=1) +
#   ylab("Ideology: Islamist v. Secularist") + xlab("Economic Policy: Left vs. Right") +
# geom_errorbarh(aes(xmin=Twod5_Perc,xmax=Twod95_Perc),alpha=0.2) + geom_hline(yintercept=0,alpha=0.5) + geom_vline(xintercept = 0,alpha=0.5) +
#   stat_ellipse(geom="polygon",alpha=0.5,aes(fill=Party2)) + scale_colour_brewer(palette="Set1")
# 
# ggplot(data=combined_legis,aes(y=Onedmean,x=Twodmean,colour=Party)) + geom_point() + my_theme + 
#   geom_text(aes(label=Onedlegis.names),check_overlap=TRUE,hjust=0.5,vjust=1) +
#   ylab("Ideology: Islamist v. Secularist") + xlab("Economic Policy: Left vs. Right") +
#   geom_errorbarh(aes(xmin=Twod5_Perc,xmax=Twod95_Perc),alpha=0.2) + geom_hline(yintercept=0,alpha=0.5) + geom_vline(xintercept = 0,alpha=0.5) +
#   scale_colour_brewer(palette="Set1")
