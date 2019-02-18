library(rethinking)
# setup prior + posterior
p_grid <- seq( from=0 , to=1 , length.out=10000)
prior <- rep(1, 10000)
likelihood <- dbinom(6, size= 9, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
dens(samples)
w <- rbinom(1e4, size = 3, prob = samples)
simplehist(w)
# calculate confidence intervals
orderd_samples <- samples[order(samples)]
orderd_samples[which(seq(orderd_samples)/length(orderd_samples) > .8)[1]]
quantile(samples, 0.1)

chainmode(samples, adj = .01)
# calculate absolute loss function
loss <- sapply(p_grid, FUN = function(x) sum(posterior*abs(x-p_grid)))
p_grid[which.min(loss)]

loss_vals <- vector(length = length(p_grid))
n <- 1
for(i in p_grid){
  loss_vals[n] <- sum(posterior*abs(i - p_grid))
  n <- n + 1
}
plot(x = p_grid, y = loss_vals)

sum(posterior*abs(0.5 - p_grid))

# calculate likelihood for sample of tosses
dbinom(x = 0:9, size = 9, prob = 0.7)

# simulate observations, using likelihoods
table(rbinom(n = 100000, size = 9, prob = 0.7))/1e5
simplehist(rbinom(n = 100000, size = 9, prob = 0.7))

# posterior predictive distribution
obs <- rbinom(1e6, size = 9, prob = samples)
obs <- lapply(seq(0, 1, 10), FUN = function(x) rbinom(1e6, 9, x))
str(obs)







