# IRT Stan example

require(rstan)
require(shinystan)

# Generate person-related parameters and predictors
J <- 500
sigma <- 1
lambda <- c(0.5, 0.5, 0.5)
W <- cbind(1, rnorm(J, 0, 1), rnorm(J, 0, 1))
theta <- W %*% matrix(lambda) + rnorm(J, 0, sigma)

# Generate item parameters
I <- 20
Beta_uncentered <- matrix(NA, nrow = I, ncol = 2)
Beta_uncentered[, 1] <- seq(from = -1.5, to = 1.5, length.out = I)
Beta_uncentered[, 2] <- Beta_uncentered[, 1] + rep(c(0.25, 0.5, 0.75, 1), length.out = I)
Beta_centered <- Beta_uncentered - mean(Beta_uncentered)

# A function to simulate responses from the model
simulate_response <- function(theta, beta) {
  unsummed <- c(0, theta - beta)
  numerators <- exp(cumsum(unsummed))
  denominator <- sum(numerators)
  response_probs <- numerators/denominator
  simulated_y <- sample(1:length(response_probs) - 1, size = 1, prob = response_probs)
  return(simulated_y)
}

# Assemble the data list to pass to Stan
data_list <- list(I = I, J = J, N = I * J, ii = rep(1:I, times = J), jj = rep(1:J, 
                                                                              each = I))
data_list$y <- numeric(data_list$N)
for (n in 1:data_list$N) {
  data_list$y[n] <- simulate_response(theta[data_list$jj[n]], Beta_centered[data_list$ii[n], 
                                                                            ])
}
data_list$K <- ncol(W)
data_list$W <- W

sim_fit <- stan(file = "pcm_latent_reg.stan", data = data_list, chains = 4, 
                iter = 200)