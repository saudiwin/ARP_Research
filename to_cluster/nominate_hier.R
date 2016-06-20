setwd("C:/Users/bobku/Box Sync/Measurement Class/Computational examples/NOMINATE")
source("C:/Users/bobku/Box Sync/Big Data/Data/R Code_preliminary implementation/R Test Data/R Scripts/Ggplot2_theme.R")
library(rstan)
library(foreign)
library(pscl)
require(anominate)
require(reshape2)
require(ggplot2)

sen <- read.dta("sen113kh.dta")
senvotes <- sen[,-c(1:9)]
rownames(senvotes) <- paste(sen$name,sen$lstate,sep=" ")
parties <- factor(sen$party)

sv.yay <- matrix(as.numeric(senvotes==1|senvotes==2|senvotes==3), nrow(senvotes), ncol(senvotes))
row.names(sv.yay) <- row.names(senvotes)
# Exclude those who vote less than 2 std. deviations away from the mean number of votes
num_votes  <- apply(sv.yay,1,sum)
names(num_votes) <- row.names(sv.yay)
row_filter <- (num_votes>(mean(num_votes)-2*sd(num_votes)))
sv.yay <- sv.yay[row_filter,]
parties <- parties[row_filter]
party_vals <- data.frame(id=1:length(parties),parties)
# Remove bills that have near-unaninous agreement
bills_agree <- apply(sv.yay,2,mean)
sv.yay <- sv.yay[,((bills_agree>0.025) & (bills_agree<0.975))]
num_legis <- nrow(sv.yay)
num_bills <- ncol(sv.yay)
legislator_points <- rep(1:num_legis,times=num_bills)
party_points <- as.integer(factor(party_vals$parties[match(legislator_points,party_vals$id)]))
# Change lowest category to base category (0)
party_points <- party_points
party_num <- length(unique(party_points))
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
  int party_num;
  int pp[N];
}

transformed data {

#    real fix1;
#  real fix2;
#  fix1 <- .75;
#  fix2 <- -.75;
}

parameters {
  vector[num_legis-2] L_open;
  vector[party_num-1] P_open;
#real<lower=0> gamma;
  vector[num_bills] B_yes;
 #vector[num_bills] B_no;
  #real w[2];
  real fix1;
  real fix2;
  real<lower=0> party_fix;
  real<lower=0> sigma;
  #real<lower=0> beta;
 #real mean_loc;
}

transformed parameters {
vector[num_legis] L;
real log_gamma;
vector[party_num] P;
  #Constraint individual legislators
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
  # Constrain individual party
  for(p in 1:party_num) {
    if(p==1) {
      P[p] <- party_fix;
    } else {
      P[p] <- P_open[p-1];
    }
  }
#   for(p in 1:num_party) {
#     if(p==1) {
#         P[p] <- 0;
#     } else {
#         P[p] <- P_open[p];
#     }
#   }
#log_gamma <- log(gamma);
}

model {	
  #Define intermediate variables
  vector[N] pi;
  real yay;
real nay;

  #priors
  sigma ~ normal(0,1);
  L_open ~ normal(0,5);
  P_open ~ normal(0,5);
  party_fix ~ normal(0,5);
  fix1 ~ normal(0,5);
  fix2 ~ normal(0,5);
  B_yes ~ normal(0,5);
  
#model
  for(n in 1:N) {
    pi[n] <- normal_log(B_yes[bb[n]],L[ll[n]] + P[pp[n]] ,1);
  }

  Y ~ bernoulli_logit(pi);
  
}
'
parameter_list <- list(gamma=1:num_bills,L_open=(1:(num_legis-2)),fix1=1,fix2=2,B_yes=1:num_bills)
create_inits <- function(x) {
  runif(length(x),min = 0.01,max=2)
}
init_list <- lapply(1:2, function(x) lapply(parameter_list,create_inits))
fit <- stan(model_name="Nominate: 1 dimension",model_code=model_code, 
            data = list(Y=Y, N=length(Y), num_legis=num_legis, num_bills=num_bills, ll=legislator_points,
                        bb=bill_points,constrain=sort(c(which(grepl('COBURN',row.names(sv.yay))),which(grepl('REID',row.names(sv.yay))))),
                        party_num=party_num,pp=party_points),
            iter=1000, chains=2,warmup=500,cores=2)
means_fit <- summary(fit)[[1]]
legis_means <- means_fit[grepl("L\\[",row.names(means_fit)),]
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

#Check correlation between estimates

cor(legis_data$coord1D,legis_data$stan_mean)

ggplot(legis_data,aes(x=coord1D,y=stan_mean)) + geom_point() +
  geom_text(aes(label=id),check_overlap = TRUE) + geom_abline(intercept=0,slope=1) + my_theme +
  geom_pointrange(aes(ymin=lowci,ymax=highci),alpha=0.5) + ylab("Stan Estimates") + xlab("WNOMINATE Estimates")

ggplot(legis_data,aes(y=reorder(id,stan_mean),x=stan_mean)) + geom_point() + my_theme +
  geom_text(aes(label=id),check_overlap=TRUE) + geom_vline(xintercept=0) + theme(axis.text.y=element_blank()) +
  geom_errorbarh(aes(xmin=lowci,xmax=highci))