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
  
  # Return list with simuulated data, true values of parameters and also list of initial starting values for Stan
  return(list(data=list(num_legis=K, num_bills=J, N=N, ll=kk, bb=jj, Y=y,data_phi=start_vals,
                        constrain_pos=constrain_pos,constrain_neg=constrain_neg,
                        num_con_pos=length(constrain_pos),num_con_neg=length(constrain_neg),
                        true_con_pos=length(constrain_pos),true_con_neg=length(constrain_neg)),
              pars=list(phi=phi,theta=theta,gamma=gamma,vote_matrix=this_matrix)))
}

simulate_nom_ideal <- function(J,K) {
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
    raw_mu <- dnorm(phi[kk[n]] - theta[jj[n]],0,1,log=TRUE)
    y[n] <- rbinom(n=1,size=1,prob = plogis(raw_mu))
  }
  
  # Create a sparse matrix with rows=J users, columns = K elites, and cells are the count of retweets
  this_matrix <- sparseMatrix(i = jj,
                              j = kk,
                              x = y)
  
  # Return list with simuulated data, true values of parameters and also list of initial starting values for Stan
  return(list(data=list(num_legis=K, num_bills=J, N=N, ll=kk, bb=jj, Y=y,data_phi=start_vals,
                        constrain_pos=constrain_pos,constrain_neg=constrain_neg,
                        num_con_pos=length(constrain_pos),num_con_neg=length(constrain_neg),
                        true_con_pos=length(constrain_pos),true_con_neg=length(constrain_neg)),
              pars=list(phi=phi,theta=theta,gamma=gamma,vote_matrix=this_matrix)))
}

simulate_data_nom_2d <- function(J,K) {
  phi1 <- rnorm(K,0,1) # ideology of legislators
  phi2 <- rnorm(K,0,1)
  theta <- rnorm(J, 0, 0.5) # difficulty parameters for bills
  gamma1 <- runif(J,.1,1.5) #discrimination
  gamma2 <- runif(J,.1,5)
  constrain_pos1 <- which(phi1>(max(phi1)-sd(phi1)))
  constrain_neg1 <- which(phi1<(min(phi1)+sd(phi1)))
  constrain_pos2 <- 0
  constrain_neg2 <- which(gamma2<(min(gamma2)+.1))
  all_cons1 <- c(constrain_pos1,constrain_neg1)
  not_phi <- phi1[-all_cons1]
  phi1 <- c(phi1[all_cons1],not_phi)
  all_cons2 <- c(constrain_pos2,constrain_neg2)
  not_gamma <- gamma2[-all_cons2]
  gamma2 <- c(gamma2[all_cons2],not_gamma)
  not_gamma <- gamma1[-all_cons2]
  gamma1 <- c(gamma1[all_cons2],not_gamma)
  jj <- rep(1:J, times=K) # twitter user for observation n
  kk <- rep(1:K, each=J) # elite account for observation n
  N <- J * K
  y <- rep(NA, N) 
  
  for (n in 1:N){ # computing p_ij
    raw_mu <- log(dnorm(theta[jj[n]],phi1[kk[n]],gamma1[jj[n]]) + 
                    dnorm(theta[jj[n]],phi2[kk[n]],gamma2[jj[n]]))
    y[n] <- rbinom(n=1,size=1,prob = plogis(raw_mu))
  }
  
  # Create a sparse matrix with rows=J users, columns = K elites, and cells are the count of retweets
  this_matrix <- sparseMatrix(i = jj,
                              j = kk,
                              x = y)
  
  # Return list with simuulated data, true values of parameters and also list of initial starting values for Stan
  return(list(data=list(num_legis=K, num_bills=J, N=N, ll=kk, bb=jj, Y=y,
                        constrain_pos1=constrain_pos1,constrain_neg1=constrain_neg1,
                        constrain_pos2=constrain_pos2,constrain_neg2=constrain_neg2,
                        num_con_pos1=length(constrain_pos1),num_con_neg1=length(constrain_neg1),
                        num_con_pos2=1,num_con_neg2=length(constrain_neg2),
                        true_con_pos1=length(constrain_pos1),true_con_neg1=length(constrain_neg1),
                        true_con_pos2=0,true_con_neg2=0),
              pars=list(phi1=phi1,phi2=phi2,theta=theta,gamma1=gamma1,
                        gamma2=gamma2,vote_matrix=this_matrix)))
}