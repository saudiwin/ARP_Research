setwd("~/Box Sync/Measurement/2014")
library(rstan)
library(foreign)

sen <- read.dta("sen112kh.dta")
senvotes <- sen[,-c(1:9)]
rownames(senvotes) <- paste(sen$name,sen$lstate,sep=" ")

sv.yay <- matrix(as.numeric(senvotes==1|senvotes==2|senvotes==3), nrow(senvotes), ncol(senvotes))
sv.nay <- matrix(as.numeric(senvotes==4|senvotes==5|senvotes==6), nrow(senvotes), ncol(senvotes))

model_code <- '
data {
  int<lower=1> S;
  int<lower=1> V;		
  real Y[S,V];
  real N[S,V];
}

parameters {
  vector[V] yay;
  vector[V] nay;
  real<lower=.01> sigma[V];
  vector[S] theta;
}

model {	
  #Define intermediate variables
  real mu;
  real pi;

  #priors
  yay ~ normal(0,5);
  nay ~ normal(0,5);
  sigma ~ normal(0,5);
  theta ~ normal(0,5);

  #model
  for(s in 1:S) {
    for(v in 1:V) {
      mu <- sigma[v]*(exp(-.5*(theta[s]-yay[v])^2)+exp(-.5*(theta[s]-nay[v])^2));
      pi <- normal_cdf(mu,0,1);
      increment_log_prob(Y[s,v]*log(pi)+N[s,v]*log(1-pi));
    }
  }
}
'

fit <- stan(model_name="Nominate: 1 dimension",model_code=model_code, 
            data = list(Y=sv.yay, N=sv.nay, S=nrow(sv.yay), V=ncol(sv.yay)), 
            iter=900, chains=3, init=0)



