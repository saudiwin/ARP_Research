data {
  int N;
  int Y[N];
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int ll[N];
  int bb[N];
  int num_parties;
  matrix[num_legis,num_parties] party_dum;
}

parameters {
  vector[num_legis] L_open;
  vector[num_bills-1] B_yes;
  vector<lower=0>[num_bills] sigma;
  vector[num_parties - 1] open_delta;
  real<lower=0> delta_1;
}

transformed parameters {

vector[num_parties] delta;
vector[num_legis] L_party;
vector[num_bills] B_adj;
	
delta[1] <- delta_1;
for(d in (1+1):num_parties) {
delta[d] <- open_delta[d-1];
}
	
L_party <- party_dum * delta;

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
  delta_1 ~ normal(0,1);
  open_delta ~ normal(0,1);

  B_yes ~ normal(0,5);
  #model
  for(n in 1:N) {
      pi[n] <-  normal_log(B_adj[bb[n]],L_party[ll[n]],sigma[bb[n]]);
  }

  Y ~ bernoulli_logit(pi);
  
}

generated quantities {
	vector[num_legis] L_std;
	
	L_std <- ((L_open + L_party) - mean(L_open + L_party))/sd(L_open + L_party);
	
	
}
