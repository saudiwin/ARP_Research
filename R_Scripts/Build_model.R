#Check whether the model is doing what we want it to

# Two latent variables, ideology and policy, which is a function of party plus an personal component

N <- 1000

party1d1 <- 1
party2d1 <- -1
party2d2 <- 0.5
party2d2 <- -0.5

bill_positions <- runif(N,-2,2)
bill_discrim <- runif(N,.1,1.5)

ideal_point1 <- 