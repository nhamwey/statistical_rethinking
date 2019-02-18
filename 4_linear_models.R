library(rethinking)
sum(ifelse(runif(16) < 0.5, -1, 1))
# random steps
dens(replicate(1e5, sum(round(runif(16, -1, 1))**2)), adj = 5)
# random multiplication       
dens(replicate(1e5, prod(1 + runif(12, 0, .1))), norm.comp = T)
# large deviates aren't normal
dens(replicate(1e5, prod(1 + runif(12, 0, .5))), norm.comp = T)
# log of large deviates are normal
dens(replicate(1e5, log(prod(1 + runif(12, 0, .5)))), norm.comp = T)

# !Kung San!
library(rethinking)
data(Howell1)
d <- Howell1
# height is correlated with age, before adulthood
d2 <- d[ d$age >= 18 , ]     
dens(d2)
curve(dnorm(x, 178, 20), from = 100, to = 250)
curve(dunif(x, 0, 50), from = -10, to = 60)
# posterior for normal
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)


#grid approximation
mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )
#view posterior
contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)
#sample from posterior
sample.rows <-sample(1:nrow(post), size = 1e4, prob = post$prob, replace = T)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

plot( sample.mu , sample.sigma , cex=1 , pch=20 , col=col.alpha(rangi2,0.1) )

dens(sample.mu)
dens(sample.sigma)

HPDI(sample.mu)

# sample only 20 observations
d3 <- sample( d2$height , size=20 )

mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
  sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
              log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
  dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                        prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="mu" , ylab="sigma" , pch=16 )

dens( sample2.sigma , norm.comp=TRUE )

# map quadratic approximation
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
)

m4.1 <- map( flist , data=d2 )
precis(m4.1)
vcov(m4.1)

# extract samples from multidimensional posterior
post <- extract.samples(m4.1, n = 1e4)
