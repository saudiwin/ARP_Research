data {
  int N;
  int Y[N];
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int ll[N];
  int bb[N];
  int num_parties;
  int business[num_legis];
  matrix[num_legis,num_parties] party_dum;
}

parameters {
  vector[num_legis] L_open;
  vector[num_bills] B_yes;
  vector<lower=0>[num_bills] sigma;
  real delta_0;
  vector[num_parties - 1] open_delta;
  real<lower=0> delta_1;
  real business_eff;
}

transformed parameters {

vector[num_parties] delta;

vector[num_legis] L_adj;
	vector[num_bills] B_adj;
	 real L_mean;
	L_mean <- mean(L_open);
	
	L_adj <- L_open - L_mean;
	B_adj <- B_yes - L_mean;

delta[1] <- delta_1;
for(d in (1+1):num_parties) {
delta[d] <- open_delta[d-1];
}

}

model {	
  #Define intermediate variables
  vector[N] pi;

  #priors
  #Sigma prior is set to cluster near zero, which is where many of the values seem to go, while also placing positive probability on much larger values of sigma*L_sd
  #for bills with low discrimination
  #Setting the prior near the likelihood values should help speed estimation, which is currently quite slow
  #sigma_mean ~ normal(0,1);
  sigma ~ lognormal(-1,2);
  business_eff ~ normal(0,1);
  L_open ~ normal(delta_0 + party_dum * delta + business_eff + (party_dum*delta) * business_eff,10);
  delta_0 ~ normal(0,5);
  delta_1 ~ normal(0,1);
  open_delta ~ normal(0,5);
  B_yes ~ normal(0,10);
  #model
  for(n in 1:N) {
      pi[n] <-  normal_log(B_adj[bb[n]],L_adj[ll[n]],sigma[bb[n]]);
  }

  Y ~ bernoulli_logit(pi);
  
}
