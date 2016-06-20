# Following is an attempted estimation of ordinal vote data from the Tunisian parliament
# Dataset has been coded as 0=missing, 1=no, 2=abstain, 3=yes

require(emIRT)

ordinal_data <- readRDS('ordinal_data.rds')

K <- ncol(ordinal_data)
J <- nrow(ordinal_data)

# Starting values created using uniform distribution for bills/MPs

start.values <- list(alpha=matrix(rep(0,K),nrow=K,ncol=1),
                     beta=matrix(runif(K,-1,1),nrow=K,ncol=1),
                     x=matrix(runif(J,-4,4),nrow=J,ncol=1),
                     DD=matrix(rep(0.5,K),nrow=K,ncol=1),
                     tau=matrix(rep(-0.5,K),nrow=K,ncol=1))

#priors match example priors

priors <- vector(mode = "list")
priors$x <- list(mu = matrix(0,1,1), sigma = matrix(1,1,1) )
priors$beta <- list(mu = matrix(0,2,1), sigma = matrix(diag(25,2),2,2))

# Results in error: 
# error: Mat::init(): size is fixed and hence cannot be changed
# Error in eval(expr, envir, enclos) : 
#   Mat::init(): size is fixed and hence cannot be changed

ord_all <- ordIRT(.rc=ordinal_data,.starts=start.values,
                  .priors=priors)

# Attempt with normally-distributed starting values

start.values <- list(alpha=matrix(rep(0,K),nrow=K,ncol=1),
                     beta=matrix(rnorm(K,0,1),nrow=K,ncol=1),
                     x=matrix(rnorm(J,0,2),nrow=J,ncol=1),
                     DD=matrix(rep(0.5,K),nrow=K,ncol=1),
                     tau=matrix(rep(-0.5,K),nrow=K,ncol=1))

#priors match example priors

priors <- vector(mode = "list")
priors$x <- list(mu = matrix(0,1,1), sigma = matrix(1,1,1) )
priors$beta <- list(mu = matrix(0,2,1), sigma = matrix(diag(25,2),2,2))

# Results in error: 
# error: Mat::init(): size is fixed and hence cannot be changed
# Error in eval(expr, envir, enclos) : 
#   Mat::init(): size is fixed and hence cannot be changed

ord_all <- ordIRT(.rc=ordinal_data,.starts=start.values,
                  .priors=priors)

# Attempt with zero starting values

start.values <- list(alpha=matrix(rep(0,K),nrow=K,ncol=1),
                     beta=matrix(rep(0,K),nrow=K,ncol=1),
                     x=matrix(rep(0,J),nrow=J,ncol=1),
                     DD=matrix(rep(0.5,K),nrow=K,ncol=1),
                     tau=matrix(rep(-0.5,K),nrow=K,ncol=1))

#priors match example priors

priors <- vector(mode = "list")
priors$x <- list(mu = matrix(0,1,1), sigma = matrix(1,1,1) )
priors$beta <- list(mu = matrix(0,2,1), sigma = matrix(diag(25,2),2,2))

# Estimates without errors

ord_all <- ordIRT(.rc=ordinal_data,.starts=start.values,
                  .priors=priors)

#BUT, all ideal points equal zero

summary(ord_all$means$x)
summary(ord_all$means$beta)

