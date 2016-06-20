# Robert Kubinec
# V0.2 Simulation of Zero-inflated IRT data

require(rstan)
require(shinystan)
require(ggplot2)
require(stringr)
require(reshape2)
require(Matrix)
require(ZIM)
require(parallel)
require(loo)
require(data.table)
source("Ggplot2_theme.R")

# Variables for MCMC algorithm
num_iters <- 500
num_burn <- 250
num_chains <- 1
num_cores <- 1
num_sims <- 2
set.seed(seed=717368)

#Init beta to be positive so as to prevent the model breaking without a link function

init_list <- lapply(1,function(x) list(beta=runif(1,0.5,1.5)))

# Simulate twitter tweet-retweet data from a set of J twitter users and K twitter "elites" where J>K

simulate_data_quad <- function(J, K){
  # J = number of twitter users
  # K = number of elite twitter accounts
  # ideology of elites 
  phi <- rnorm(K,0,1) # difficulty parameters
  theta <- rnorm(J, 0, 0.5) # ideology of users
  beta <- 1 #follower parameters
  gamma <- runif(1,1,5) #discrimination
  mu_shape <- .01
  jj <- rep(1:J, times=K) # twitter user for observation n
  kk <- rep(1:K, each=J) # elite account for observation n
  N <- J * K
  followers <- round(runif(J,min=100,max=10000))/1000
  y <- rep(NA, N) 
  
  # Need to construct our own way to simulate a hurdle model
#   for(n in 1:N) {
#     type <- rbinom(1,1,plogis(-beta*followers[n] + gamma[kk[n]] * (theta[jj[n]] - phi[kk[n]])^2) )
#     y[n] <- if(type==0) {
#       0
#     } else {
#       rnbinom(n=1,mu=exp(beta * followers[n] - gamma[kk[n] + 10] * (theta[jj[n]] - phi[kk[n]])^2),size=mu_shape) 
#     }
#     
#   }
  
  for (n in 1:N){ # computing p_ij
    raw_mu <- beta * followers[jj[n]] + gamma *pnorm( theta[jj[n]], phi[kk[n]])
    computed_mu <- exp(raw_mu)
    ideology <- gamma *pnorm( theta[jj[n]], phi[kk[n]])
    follower_value <- beta * followers[n]
    r <- as.integer(rnbinom(n=1,mu= computed_mu,size=mu_shape))
    while(is.nan(r) | is.na(r)) {
      print(paste0("Problematic values are: shape ",round(mu_shape,digits = 2)," gamma ",round(gamma,digits=2),
                   " theta ", round(theta[jj[n]],digits=2)," beta ",beta," followers ", followers[n], " phi ",round(phi[kk[n]],digits=2),
                   " ideology ",ideology," follower value ",follower_value," raw value ",raw_mu," and total mean value ",round(computed_mu,digits=3),"\n"))
      r <- as.integer(rnbinom(n=1,mu=computed_mu,size=mu_shape[jj[n]]))
    }
    y[n] <- r
  }
  
  
#   for (n in 1:N){ # computing p_ij
#     raw_mu <- beta * followers[jj[n]] - gamma[kk[n]] *( theta[jj[n]] - phi[kk[n]])^2
#     computed_mu <- exp(beta * followers[jj[n]] - gamma[kk[n]] *( theta[jj[n]] - phi[kk[n]])^2)
#     ideology <- - gamma[kk[n]] *( theta[jj[n]] - phi[kk[n]])^2
#     follower_value <- beta * followers[n]
#     r <- as.integer(rnbinom(n=1,mu= computed_mu,size=mu_shape[jj[n]]))
#     while(is.nan(r) | is.na(r)) {
#       print(paste0("Problematic values are: shape ",round(mu_shape[jj[n]],digits = 2)," gamma ",round(gamma,digits=2),
#                    " theta ", round(theta[jj[n]],digits=2)," beta ",beta," followers ", followers[n], " phi ",round(phi[kk[n]],digits=2),
#                    " ideology ",ideology," follower value ",follower_value," raw value ",raw_mu," and total mean value ",round(computed_mu,digits=3),"\n"))
#       r <- as.integer(rnbinom(n=1,mu=computed_mu,size=mu_shape[jj[n]]))
#     }
#     y[n] <- r
#   }
  
    # Create a sparse matrix with rows=J users, columns = K elites, and cells are the count of retweets
    this_matrix <- sparseMatrix(i = jj,
                               j = kk,
                               x = y)
    # The following code is used by Barbera (2015) to generate "reasonable" starting values for MCMC.
    # In practice it seems to be less than useful.
    colK <- colSums(this_matrix)
    rowJ <- rowSums(this_matrix)
    normalize <- function(x){ (x-mean(x))/sd(x) }
    inits <- rep(list(list(alpha=normalize(log(colK+0.0001)), 
                           beta=normalize(log(rowJ+0.0001)),
                           theta=rnorm(J), phi=phi,mu_beta=0, sigma_beta=1, 
                           gamma=abs(rnorm(1)), mu_phi=0, sigma_phi=1, sigma_alpha=1)))
    
    # Return list with simuulated data, true values of parameters and also list of initial starting values for Stan
    return(list(data=list(num_legis=J, num_bills=K, N=N, ll=jj, bb=kk, Y=y,data_phi=theta,
                follow=followers,constrain=sort(c(which(theta==min(theta)),which(theta==max(theta))))),
                pars=list(phi=phi,theta=theta,gamma=gamma,beta=beta,mu_shape=mu_shape),inits=inits))

}

simulate_data_nom <- function(J,K) {
  phi <- rnorm(K,0,1) # ideology of legislators
  theta <- rnorm(J, 0, 0.5) # difficulty parameters for bills
  gamma <- runif(J,.1,1.5) #discrimination
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
  # The following code is used by Barbera (2015) to generate "reasonable" starting values for MCMC.
  # In practice it seems to be less than useful.
  colK <- colSums(this_matrix)
  rowJ <- rowSums(this_matrix)
  normalize <- function(x){ (x-mean(x))/sd(x) }
  inits <- rep(list(list(
                         theta=rnorm(J), phi=phi,
                         gamma=abs(rnorm(J)))))
  
  # Return list with simuulated data, true values of parameters and also list of initial starting values for Stan
  return(list(data=list(num_legis=K, num_bills=J, N=N, ll=kk, bb=jj, Y=y,data_phi=phi,
              constrain=sort(c(which(phi==min(phi)),which(phi==max(phi))))),
              pars=list(phi=phi,theta=theta,gamma=gamma),inits=inits))
}

# Load Stan code files by matching pattern
all_model_types <- list.files(pattern="nominate_test",full.names=TRUE)
compile_model <- function(x) {
  stan_file <- readChar(x, file.info(x)$size)
  stan_model(model_code=stan_file,model_name="Test")
}
#Produce compiled models 
compiled_models <- lapply(all_model_types,compile_model)
# One set of test data for all simulations
test_data <- simulate_data_nom(100,10)
if(any(is.na(test_data[[1]]$Y))) {
  stop("There are missing values in Y, which is impossible for a negative binomial model.")
} 

ci_05 <- function(x) {
  quantile(x,0.05,na.rm=TRUE)
}
ci_95 <- function(x) {
  quantile(x,0.95,na.rm=TRUE)
}

# Loop over Stan code files, produce Stan object as output

over_model_types <- function(x) {
    over_iterations <- function(i,this_model) {

    stan.fit <- sampling(compiled_models[[x]],data = test_data[[1]], 
                     iter=num_iters, warmup=num_burn, chains=num_chains,cores=num_cores, 
                     refresh=50)
    stan.fit
    }
    
    all_models <- lapply(1:num_sims,over_iterations,run_model)
    # Create shinystan objects for summary
    samples <- lapply(all_models,function(x) summary(x)[[1]])
    param_names <- row.names(samples[[1]])
    keep_names <- grepl("L_adj\\[",param_names)
    samples <- lapply(samples,function(x) as.data.table(x[keep_names,]))
    sample_table <- rbindlist(samples)

    # Make a fake table to append true values
    test_data <- data.table("mean"=test_data[[1]]$data_phi)
    sample_table <- rbind(sample_table,test_data,fill=TRUE)
    sample_table$type <- c(rep("Simulation",10*num_sims),rep("True",10))
    sample_table$id <- param_names[keep_names]
    outplot <- ggplot(data=sample_table,aes(y=mean,x=id,shape=type,colour=type,size=0.3)) + geom_jitter(width=0.25) + my_theme +
      geom_violin(size=0.3,colour="black",fill=NA,alpha=0.5)
    outplot
    
    
}
outobj <- lapply(1:length(all_model_types),over_model_types)

