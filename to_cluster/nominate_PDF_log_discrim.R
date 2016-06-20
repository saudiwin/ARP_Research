setwd("C:/Users/bobku/Box Sync/Measurement Class/Computational examples/NOMINATE")
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

sv.yay <- matrix(as.numeric(senvotes==1|senvotes==2|senvotes==3), nrow(senvotes), ncol(senvotes))
row.names(sv.yay) <- row.names(senvotes)
# Exclude those who vote less than 2 std. deviations away from the mean number of votes
num_votes  <- apply(sv.yay,1,sum)
names(num_votes) <- row.names(sv.yay)
sv.yay <- sv.yay[(num_votes>(mean(num_votes)-2*sd(num_votes))),]
# Remove bills that have near-unaninous agreement
bills_agree <- apply(sv.yay,2,mean)
#sv.yay <- sv.yay[,((bills_agree>0.025) & (bills_agree<0.975))]

# put constraints into the model. For this model, one each for democrats and republicans

constrain_legis <- sort(c(which(grepl('COBURN',row.names(sv.yay))),which(grepl('REID',row.names(sv.yay)))))

# Reorder points which to use for constraining so that the constraints are always first.

num_legis <- nrow(sv.yay)
num_bills <- ncol(sv.yay)
legislator_points <- rep(1:num_legis,times=num_bills)
bill_points <- rep(1:num_bills,each=num_legis)
Y <- c(sv.yay)



model_code <- '
data {
  int N;
  int Y[N];
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int ll[N];
  int bb[N];
  int constrain[2];
}

parameters {
  vector[num_legis-2] L_open;
  #real<lower=0> gamma;
  vector[num_bills] B_yes;
  real fix1;
  real<lower=0> fix2;
  vector<lower=0>[num_bills] sigma;
}

transformed parameters {
vector[num_legis] L;
vector[num_legis] L_adj;
vector[num_bills] B_adj;
vector[num_bills] S_adj;
real L_mean;
real L_sd;
  for(l in 1:(constrain[1] - 1)) {
    L[l] <- L_open[l];
  }
  L[constrain[1]] <- fix1;
  for(l in (constrain[1]+1):(constrain[2]-1)) {
    L[l] <- L_open[l-1];
  }
  L[constrain[2]] <- fix2;
  for(l in (constrain[2]+1):(num_legis)) {
    L[l] <- L_open[l-2];
  }
L_mean <- mean(L);
L_sd <- sd(L);
L_adj <- (L - L_mean)/L_sd;
B_adj <- (B_yes - L_mean)/L_sd;
S_adj <- sigma*L_sd;
}

model {	
  #Define intermediate variables
  vector[N] pi;

  #priors
  #sigma ~ normal(0,1);
  L_open ~ normal(0,5);
  fix1 ~ normal(0,5);
  fix2 ~ normal(0,5);
  B_yes ~ normal(0,5);
  #model
  for(n in 1:N) {
      pi[n] <- normal_log(B_adj[bb[n]],L_adj[ll[n]],S_adj[bb[n]]);
  }

  Y ~ bernoulli_logit(pi);
  
}
'
#Parameter of inference is L_adj (legislator points) and B_adj (bill points). The legislature and bill ideal points have been adjusted by 
#Subtracting the mean of the vector of legislature ideal points. This is recommended by Gelman (2005) to speed 
#up convergence, and indeed it does so. 

compiled_model <- stan_model(model_code=model_code,model_name="Nominate: 1 dimension")
sample_fit <- sampling(object=fit,
            data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                        bb=bill_points,constrain=constrain_legis),
            iter=500, chains=2,warmup=250,cores=2)
means_fit <- summary(sample_fit)[[1]]
legis_means <- means_fit[grepl("L_adj\\[",row.names(means_fit)),]

#Test with wnominate

vote_data <- list(votes=sv.yay,legis.names=row.names(sv.yay))
vote_data <- rollcall(data=vote_data)

wnom_fit <- wnominate(vote_data,polarity=which(grepl('FRANKEN',sen$name)),dims=1,minvotes=100)
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

# Plot Stan version points as a summary

ggplot(legis_data,aes(y=reorder(id,stan_mean),x=stan_mean)) + geom_point() + my_theme +
  geom_text(aes(label=id),check_overlap=TRUE) + geom_vline(xintercept=0) + theme(axis.text.y=element_blank()) +
  geom_errorbarh(aes(xmin=lowci,xmax=highci))