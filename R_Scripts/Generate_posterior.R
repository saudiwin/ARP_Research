# Given a sample_fit object, we want to generate the posterior predictive distribution

params <- rstan::extract(sample_fit)
num_legis <- nrow(vote_matrix)
num_bills <- ncol(vote_matrix)
legislator_points <- rep(1:num_legis,times=num_bills)
bill_points <- rep(1:num_bills,each=num_legis)
N <- length(bill_points)
to_use <- sample(1:nrow(params[[1]]),200)

post_predict3 <- sapply(to_use,function(x) {
  pi1 <- matrix(nrow=N,ncol=ncol(params$steps_votes)+1)
  pi2 <- 1:N
  #runif1 <- runif(n = N)
  #runif2 <- runif(n=N)
  for(n in 1:N) {
    pi2[n] <- plogis(params$sigma_abs_open[x,bill_points[n]]*params$L_open[x,legislator_points[n]] - 
                                   params$B_abs[x,bill_points[n]] + params$avg_particip[x] * participation$particip_rate[legislator_points[n]])
    for(s in 1:(ncol(params$steps_votes)-1)) {
      pi1[n,s+1] <- plogis(params$sigma_adj[x,bill_points[n]]*params$L_open[x,legislator_points[n]] - 
                             params$B_yes[x,bill_points[n]] - params$steps_votes[x,s]) - 
        plogis(params$sigma_adj[x,bill_points[n]]*params$L_open[x,legislator_points[n]] - 
                 params$B_yes[x,bill_points[n]] - params$steps_votes[x,s+1])
    }
    pi1[n,1] <- 1 - plogis(params$sigma_adj[x,bill_points[n]]*params$L_open[x,legislator_points[n]] - 
                             params$B_yes[x,bill_points[n]] - params$steps_votes[x,1])
    pi1[n,ncol(params$steps_votes)+1] <- plogis(params$sigma_adj[x,bill_points[n]]*params$L_open[x,legislator_points[n]] - 
                                                  params$B_yes[x,bill_points[n]] - params$steps_votes[x,2])
  }
  outcome <- 1:N
  pi1 <- apply(pi1,2,function(x) x * (1-pi2))
  pi_full <- cbind(pi1,pi2)
  for(n in 1:N) {
      outcome[n] <- sample(1:ncol(pi_full),size=1,prob=pi_full[n,])
  }
  print(paste0('Finished iteration ',x))
  return(outcome)
})
all_counts <- table(Y)
correct_cat <- apply(post_predict3,2,function(x) {
  each_cat <- sapply(1:length(all_counts),function(y) {
    correct <- sum(Y[Y==y]==x[Y==y])
  })
  each_cat <- each_cat/all_counts
  return(each_cat)
})
(rowMeans(correct_cat))
sum((rowMeans(correct_cat)) * (all_counts/length(Y)))

lambdas <-  apply(post_predict3,2,function(x) {
  Lambda(x,Y)
})

gammas <- apply(post_predict3,2,function(x) {
  GoodmanKruskalGamma(x,Y)
})