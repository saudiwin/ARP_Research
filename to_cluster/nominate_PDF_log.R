source("Ggplot2_theme.R")
library(rstan)
library(foreign)
library(pscl)
require(anominate)
require(reshape2)
require(ggplot2)

sen <- read.dta("sen113kh.dta")
senvotes <- sen[,-c(1:9)]
rownames(senvotes) <- paste(sen$name,sen$lstate,sep=" ")
rownames(sen) <- paste(sen$name,sen$lstate,sep=" ")
sv.yay <- matrix(as.numeric(senvotes==1|senvotes==2|senvotes==3), nrow(senvotes), ncol(senvotes))
row.names(sv.yay) <- row.names(senvotes)
# Exclude those who vote less than 2 std. deviations away from the mean number of votes
num_votes  <- apply(sv.yay,1,sum)
names(num_votes) <- row.names(sv.yay)
sv.yay <- sv.yay[(num_votes>(mean(num_votes)-2*sd(num_votes))),]
# Remove bills that have near-unaninous agreement
bills_agree <- apply(sv.yay,2,mean)
sv.yay <- sv.yay[,((bills_agree>0.025) & (bills_agree<0.975))]
# Get the other data on the same scale as sv.yay
sen <- sen[row.names(sv.yay),]
# put constraints into the model. For this model, one each for democrats and republicans
#constrain_pos <- sample(c(which(sen$party==200)),size=10)
constrain_pos <- c(which(grepl('SESSIONS',row.names(sv.yay))),
                   which(grepl('LEE',row.names(sv.yay))),
                   which(grepl('INHOFE',row.names(sv.yay))),
                   which(grepl('RISCH',row.names(sv.yay))),
                   which(grepl('CRAPO',row.names(sv.yay))))
# ,which(grepl('INHOFE',row.names(sv.yay))),
# which(grepl('LEE',row.names(sv.yay))),which(grepl('RISCH',row.names(sv.yay))),
# which(grepl('MORAN',row.names(sv.yay))))
constrain_neg <- c(which(grepl('WARREN',row.names(sv.yay))),
                   which(grepl('REID',row.names(sv.yay))),
                   which(grepl('WYDEN',row.names(sv.yay))),
                   which(grepl('SCHATZ',row.names(sv.yay))),
                   which(grepl('MENENDEZ',row.names(sv.yay))))

num_con_pos <- ifelse(is.null(constrain_pos),1,length(constrain_pos))
num_con_neg <- ifelse(is.null(constrain_neg),1,length(constrain_neg))
true_con_pos <- ifelse(is.null(constrain_pos),0,length(constrain_pos))
true_con_neg <- ifelse(is.null(constrain_neg),0,length(constrain_neg))
constrain_legis <- c(constrain_pos,constrain_neg)
all_legis <- 1:nrow(sv.yay)
not_legis <- all_legis[-constrain_legis]
new_legis <- c(constrain_legis,not_legis)
sv.yay <- sv.yay[new_legis,]
num_legis <- nrow(sv.yay)
num_bills <- ncol(sv.yay)
legislator_points <- rep(1:num_legis,times=num_bills)
bill_points <- rep(1:num_bills,each=num_legis)

# Reorder points which to use for constraining so that the constraints are always first.


Y <- c(sv.yay)
script_file <- "nominate.h"
model_code <- readChar(script_file,file.info(script_file)$size)

#Parameter of inference is L_adj (legislator points) and B_adj (bill points). The legislature and bill ideal points have been adjusted by 
#Subtracting the mean of the vector of legislature ideal points. This is recommended by Gelman (2005) to speed 
#up convergence, and indeed it does so. 

compiled_model <- stan_model(model_code=model_code,model_name="Nominate: 1 dimension")
sample_fit <- sampling(object=compiled_model,
            data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                        bb=bill_points,num_con_pos=length(constrain_pos),num_con_neg=length(constrain_neg)),
            iter=1000, chains=10,warmup=500,cores=10)
saveRDS(sample_fit,"nominate_test.rds")
means_fit <- summary(sample_fit)[[1]]
legis_means <- means_fit[grepl("L_adj\\[",row.names(means_fit)),]

#Test with wnominate

vote_data <- list(votes=sv.yay,legis.names=row.names(sv.yay))
vote_data <- rollcall(data=vote_data)

wnom_fit <- wnominate(vote_data,polarity=1,dims=1,minvotes=100)
legis_data <- wnom_fit$legislators
legis_data$stan_mean <- legis_means[,1]
legis_data$lowci <- legis_means[,4]
legis_data$highci <- legis_means[,8]
legis_data$scale_mean <- scale(legis_means[,1])
legis_data$id <- row.names(legis_data)

legis_melt <- melt(legis_data,measure.vars=c("coord1D","stan_mean"))

#Check correlation between estimates. It is generally correlating at 0.96

cor(legis_data$coord1D,legis_data$stan_mean)

# Plot W-nominate points against Stan version. Notable difference is that W-Nominate over-predicts variation in 
# The democratic party, whereas the Stan version puts the Democrats all in the same box.

ggplot(legis_data,aes(x=coord1D,y=stan_mean)) + geom_point() +
  geom_text(aes(label=id),check_overlap = TRUE) + geom_abline(intercept=0,slope=1) + my_theme +
  geom_pointrange(aes(ymin=lowci,ymax=highci),alpha=0.5) + ylab("Stan Estimates") + xlab("WNOMINATE Estimates")
ggsave("stan_legis.pdf")
# Plot Stan version points as a summary

ggplot(legis_data,aes(y=reorder(id,stan_mean),x=stan_mean)) + geom_point() + my_theme +
  geom_text(aes(label=id),check_overlap=TRUE) + geom_vline(xintercept=0) + theme(axis.text.y=element_blank()) +
  geom_errorbarh(aes(xmin=lowci,xmax=highci))

ggsave("stan_v_nominate.pdf")
