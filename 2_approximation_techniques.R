# install.packages(c("coda","mvtnorm","devtools"))
# library(devtools)
# devtools::install_github("rmcelreath/rethinking")

library(rethinking)

## R code 2.3
# define grid
p_grid <- seq( from=0 , to=1 , length.out=1000)

# define prior
prior <- rep(1, 1000)

# compute likelihood at each value in grid
likelihood <- dbinom(6, size=9, prob=p_grid)
plot(x = p_grid, y = likelihood)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

## R code 2.4
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "1000 points" )

## R code 2.6
library(rethinking)
globe.qa <- map(
  alist(
    w ~ dbinom(9,p) ,  # binomial likelihood
    p ~ dunif(0,1)     # uniform prior
  ) ,
  data=list(w=6) )

# display summary of quadratic approximation
precis( globe.qa )
