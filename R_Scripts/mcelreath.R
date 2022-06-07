p_grid <- seq(from=0,to=1,length.out = 1000)
prior <- rep(1,1000)
likelihood <- dbinom(6,size=9,prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid,prob=posterior,size=1e4,replace=TRUE)
plot(samples)

library(rethinking)
dens(samples)

sum(posterior[p_grid<0.5])
sum(samples <0.5) / 1e4

PI(samples,prob=0.5)
HPDI(samples,prob=0.5)
