data {
  int N;
  int Y[N];
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int ll[N];
  int bb[N];
  int num_con_pos;
  int num_con_neg;
  int true_con_pos;
  int true_con_neg;
}

parameters {
  vector[num_legis-(true_con_pos + true_con_neg)] L_open;
  #real gamma;
  vector[num_bills] B_yes;
  vector[num_con_pos] con_pos;
  vector[num_con_neg] con_neg;
  vector<lower=0>[num_bills] sigma;
}

transformed parameters {
vector[num_legis] L;
vector[num_legis] L_adj;
vector[num_bills] B_adj;
vector[num_bills] S_adj;
  real L_mean;
real L_sd;

  if(true_con_pos>0) {
	  for(l in 1:(num_con_pos)) {
		  L[l] <- con_pos[l];
	  }
  }
  if((true_con_neg>0) && (true_con_pos==0)) {
	  for(l in 1:num_con_neg) {
		  L[l] <- con_neg[l];
	  }
  } else if((true_con_neg>0)&& (true_con_pos>0)) { 
  	  for(l in (num_con_pos+1):(num_con_neg+num_con_pos)) {
		  L[l] <- con_neg[l-num_con_pos];
	  }
  }
  for(l in (true_con_pos+true_con_neg+1):num_legis) {
	  L[l] <- L_open[l-true_con_neg-true_con_pos];
  }
L_mean <- mean(L);
L_adj <- L - L_mean;
B_adj <- B_yes - L_mean;
L_sd <- sd(L);
L_adj <- (L - L_mean)/L_sd;
B_adj <- (B_yes - L_mean)/L_sd;
S_adj <- sigma*L_sd;
}

model {	
  #Define intermediate variables
  vector[N] pi;

  #priors
  #sigma ~ normal(0,5);
  sigma ~ lognormal(0,2);
  L_open ~ normal(0,5);
	con_pos ~ normal(1,.01);
	con_neg ~ normal(-1,.01);
  B_yes ~ normal(0,5);
  #model
  for(n in 1:N) {
      pi[n] <- normal_log(B_adj[bb[n]],L_adj[ll[n]],S_adj[bb[n]]);
	  #pi[n] <- normal_log(B_adj[bb[n]],L_adj[ll[n]],1);
  }

  Y ~ bernoulli_logit(pi);
  
}
