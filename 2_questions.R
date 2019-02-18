library(rethinking)

## 2M1
# define grid
p_grid <- seq( from=0 , to=1 , length.out=1000)

# define prior
prior <- rep(0.5, 1000)

# compute likelihood at each value in grid
likelihood <- dbinom(5, size=7, prob=p_grid)
plot(x = p_grid, y = likelihood)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "1000 points" )

## 2M2
p_grid <- seq( from=0 , to=1 , length.out=1000)

prior <- ifelse(p_grid < 0.5, 0, 1)

# compute likelihood at each value in grid
likelihood <- dbinom(5, size=7, prob=p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "1000 points" )

0.15/0.65
