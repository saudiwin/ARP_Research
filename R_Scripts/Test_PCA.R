N <- 1000
jratio <- numeric(length=N)
kratio <- numeric(length=N)
gratio <- numeric(length=N)
output_corr <- numeric(length=N)
output_vars <- numeric(length=N)
simulate_data_nom <- function(J,K) {
  phi <- rnorm(K,0,1) # ideology of legislators
  theta <- rnorm(J, 0, 0.5) # difficulty parameters for bills
  gamma <- runif(J,.1,1.5) #discrimination
  constrain_pos <- which(phi>(max(phi)-sd(phi)))
  constrain_neg <- which(phi<(min(phi)+sd(phi)))
  all_cons <- c(constrain_pos,constrain_neg)
  not_phi <- phi[-all_cons]
  phi <- c(phi[all_cons],not_phi)
  jj <- rep(1:J, times=K) # twitter user for observation n
  kk <- rep(1:K, each=J) # elite account for observation n
  N <- J * K
  y <- rep(NA, N) 
  
  for (n in 1:N){ # computing p_ij
    raw_mu <- dnorm(theta[jj[n]],phi[kk[n]],gamma[jj[n]],log=TRUE)
    y[n] <- rbinom(n=1,size=1,prob = plogis(raw_mu))
  }
  
  # Create a sparse matrix with rows=J users, columns = K elites, and cells are the count of retweets
  this_matrix <- sparseMatrix(i = jj,
                              j = kk,
                              x = y)
  
  # Access principal components to get starting values for algorithm
#   pca_results <- prcomp(as.matrix(t(this_matrix)))
#   # check correlation. it seems to be highest for second principal component
#   corrs <- apply(pca_results$x,2,cor,gamma)
#   #Take principal component with highest correlation
#   high_corr <- which(corrs==max(corrs[1:5]))
#   start_vals <- scales::rescale(pca_results$x[,high_corr],to=c(-2,2))
#   print(paste0("Selecting principal component number ",high_corr," with correlation ", max(corrs[1:5])))
  
  
  # Return list with simuulated data, true values of parameters and also list of initial starting values for Stan
  return(list(data=list(num_legis=K, num_bills=J, N=N, ll=kk, bb=jj, Y=y,data_phi=start_vals,
                        constrain_pos=constrain_pos,constrain_neg=constrain_neg,
                        num_con_pos=length(constrain_pos),num_con_neg=length(constrain_neg),
                        true_con_pos=0,true_con_neg=0),
              pars=list(phi=phi,theta=theta,gamma=gamma,vote_matrix=this_matrix)))
}

for(n in 1:N) {
  J <- round(runif(1,50,200))
  K <- round(runif(1,50,200))
  
  data_test <- simulate_data_nom(J,K)
  pca_results <- prcomp(as.matrix(t(data_test[[2]]$vote_matrix)))
    # check correlation. it seems to be highest for second principal component
    corrs <- apply(pca_results$x,2,cor,data_test[[2]]$phi)
    #Take principal component with highest correlation
    high_corr1 <- which(corrs==max(corrs[1:5]))
    high_val1 <- max(corrs[1:5])
    pca_results <- prcomp(as.matrix(data_test[[2]]$vote_matrix))
    # check correlation. it seems to be highest for second principal component
    corrs <- apply(pca_results$x,2,cor,data_test[[2]]$theta)
    high_corr2 <- which(corrs==max(corrs[1:5]))
    pca_results <- prcomp(as.matrix(data_test[[2]]$vote_matrix))
    # check correlation. it seems to be highest for second principal component
    corrs <- apply(pca_results$x,2,cor,data_test[[2]]$gamma)
    #Take principal component with highest correlation
    high_corr3 <- which(corrs==max(corrs[1:5]))
    high_val2 <- max(corrs[1:5])
    kratio[n] <- high_corr1
    jratio[n] <- high_corr2
    gratio[n] <- high_corr3
    output_corr[n] <- abs(high_val1)/abs(high_val2)
    output_vars[n] <- K/J
}

plot(output_corr,output_vars)

#see how the data does
to_model <- data.frame(kratio=factor(kratio),output_vars)
model1 <- MASS::polr(formula=kratio~output_vars,data=to_model)
exp(cbind(OR=coef(model1),confint.default(model1)))

to_model <- data.frame(jratio=factor(jratio),output_vars)
model1 <- MASS::polr(formula=jratio~output_vars,data=to_model)
exp(cbind(OR=coef(model1),confint.default(model1)))

# predict whether the ratios are the same as a function of the ratio of J/K
to_model <- data.frame(true_false=(kratio==jratio),predictor=output_vars)
model_bin <- glm(true_false~output_vars,data=to_model,family="binomial")
exp(cbind(OR=coef(model_bin),confint.default(model_bin)))

aggregate(formula=output_vars~kratio,data=to_model,FUN=mean)
