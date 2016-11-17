data {
  int N;
  int Y[N];
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int ll[N];
  int bb[N];
  int opp_num;
  int gov_num;
  
}

transformed data {
	  int m;                         // # steps
	  vector[2] fix_pos; //type of fixed bills
  m = max(Y);
  fix_pos[1] = -1;
  fix_pos[2] = 1;
}

parameters {
  vector[num_legis] L_free;
  vector[num_bills] B_yes;
  vector[num_bills-(opp_num+gov_num)] sigma;
  vector[opp_num] sigma_opp;
  vector<upper=0>[gov_num] sigma_gov;
ordered[m-1] steps_free;
}

transformed parameters {

vector[num_bills] B_adj;
vector[num_bills] sigma_adj;
vector[num_legis] L_open;
/* Identification is based on the voting decisions of the last legislator in the legislator vector
 That legislator's ideal point is fixed at 1, and 10 bills are fixed at values reflecting that legislator's
 Votes, i.e., 1=yes, 0.5=absent, -0.5=abstain, -1=no
 The relative success of this identification strategy depends on the information in the 10 constrained bills
 However, even with weak information, 10 bills does appear to constrain adequately the legislator ideal points,
 Even if it does not achieve perfect identification for all bills. */
//B_adj = append_row(B_yes,rep_vector(-1*sum(B_yes),1));
/*
sigma_adj[1:(num_bills-(gov_num+opp_num+1))] = sigma;
sigma_adj[(1+num_bills-(gov_num+opp_num+1)):(num_bills-opp_num-1)] = sigma_gov;
sigma_adj[(1+num_bills-opp_num-1):(num_bills-1)] = sigma_opp;
sigma_adj[num_bills] = 0; */

sigma_adj[1:(num_bills-(gov_num+opp_num))] = sigma;
sigma_adj[(1+num_bills-(gov_num+opp_num)):(num_bills-opp_num)] = sigma_gov;
sigma_adj[(1+num_bills-opp_num):(num_bills)] = sigma_opp;

// steps = append_row(steps_free,rep_vector(-1*sum(steps_free),1));
//L_open = append_row(L_free,rep_vector(0,1));
L_open = L_free;
}

model {	
  //priors
  vector[N] pi;
  //Sigma prior is set to cluster near zero, which is where many of the values seem to go, while also placing positive probability on much larger values of sigma
  //for bills with low discrimination
  //sigmas without restrictions
  sigma ~ normal(0,5);
  //constrained positive
  sigma_opp ~ normal(0,5);
  //constrained negative
  sigma_gov ~normal(0,5);
  L_free ~ normal(0,1);
  /* We set individual priors on the steps_free cutpoints to prevent cutpoint collapse.
   The priors are set on differences to ensure spacing
   The priors also take in our subjective beliefs about the distribution of cutpoints when it comes to voting
   I.e., we believe that each of these categories represents different utility positions even if there aren't many votes
   Half of the standard deviation of the ideal points
   Think about an empirical bayesian approach to figure out cutpoint differences (in terms of the latent variable)
   */
   steps_free[2] - steps_free[1] ~ normal(1,10) T[0,];
	//  steps_free[3]  - steps_free[2] ~ normal(5,.01);
	
  B_yes ~ normal(0,5);

  //model
  for(n in 1:N) {
      pi[n] = sigma_adj[bb[n]] * ( L_open[ll[n]] - B_yes[bb[n]]);
	    Y[n] ~ ordered_logistic(pi[n],steps_free);
  }


  
}

