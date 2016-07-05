data {
  int N;
  int Y[N];
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int ll[N];
  int bb[N];
}

parameters {
  vector[num_legis] L_open;
  vector[num_bills-1] B_yes;
  vector<lower=0>[num_bills] sigma;
}

transformed parameters {

vector[num_bills] B_adj;

# Use edstan style of identification, which seems quite quick
B_adj <- append_row(B_yes, rep_vector(-1*sum(B_yes), 1));
}

model {	
  #priors
  vector[N] pi;
  #Sigma prior is set to cluster near zero, which is where many of the values seem to go, while also placing positive probability on much larger values of sigma
  #for bills with low discrimination
  
  sigma ~ lognormal(-0.5,2);
  L_open ~ normal(0,1);

  B_yes ~ normal(0,5);
  #model
  for(n in 1:N) {
      pi[n] <-  normal_log(B_adj[bb[n]],L_open[ll[n]],sigma[bb[n]]);
  }

  Y ~ bernoulli_logit(pi);
  
}

generated quantities {
	vector[num_legis] L_std;
	
	L_std <- L_open - mean(L_open);
	
	
	
}
