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
 # vector<lower=0>[num_bills] sigma;
}

transformed parameters {
vector[num_legis] L;
vector[num_legis] L_adj;
vector[num_bills] B_adj;
#vector[num_bills] S_adj;
real L_mean;
#real L_sd;
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
L_adj <- L - L_mean;
B_adj <- B_yes - L_mean;
#S_adj <- sigma*L_sd;
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
      pi[n] <- normal_log(B_adj[bb[n]],L_adj[ll[n]],1);
  }

  Y ~ bernoulli_logit(pi);
  
}
