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

# height covaration with weight
plot(d2$height ~ d2$weight)

# linear model of height and weight
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
m4.3 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 156 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d2 )

precis(m4.3)
# center weight
d2$weight.c <- d2$weight - mean(d2$weight)
mean(d2$weight.c)
# refit model with centered weight predictor
m4.4 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight.c ,
    a ~ dnorm( 156 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d2 )
precis(m4.4, corr = T)
# plot MAP value
plot( height ~ weight , data=d2 )
abline( a=coef(m4.3)["a"] , b=coef(m4.3)["b"] )
# sample from posterior
post <- extract.samples(m4.3)

# start with 10 samples
N <- 100
dN <- d2[ 1:N , ]
mN <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=dN )

## R code 4.49
# extract 20 samples from the posterior
post <- extract.samples( mN , n=1000 )

# display raw data and sample size
plot( dN$weight , dN$height ,
      xlim=range(d2$weight) , ylim=range(d2$height) ,
      col=rangi2 , xlab="weight" , ylab="height" )
mtext(concat("N = ",N))

# plot the lines, with transparency
for ( i in 1:20 )
  abline( a=post$a[i] , b=post$b[i] , col=col.alpha("black",0.3) )

mu_at_50 <- post$a + post$b * 50
dens(mu_at_50)
HPDI(mu_at_50, prob = 0.89)

# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
weight.seq <- seq( from=25 , to=70 , by=1 )
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )
str(mu)

## R code 4.55
# use type="n" to hide raw data
plot( height ~ weight , d2 , type="n" )

# loop over samples and plot each mu value
for ( i in 1:100 )
  points( weight.seq , mu[i,] , pch=16 , col=col.alpha(rangi2,0.1) )

## R code 4.56
# summarize the distribution of mu
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

sim.height <- sim( m4.3 , data=list(weight=weight.seq) )
str(sim.height)

height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

## R code 4.61
# plot raw data
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5) )

# draw MAP line
lines( weight.seq , mu.mean )

# draw HPDI region for line
shade( mu.HPDI , weight.seq )

# draw PI region for simulated heights
shade( height.PI , weight.seq )

sim.height <- sim( m4.3 , data=list(weight=weight.seq) , n=1e4 )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

## R code 4.63
post <- extract.samples(m4.3)
weight.seq <- 25:70
sim.height <- sapply( weight.seq , function(weight)
  rnorm(
    n=nrow(post) ,
    mean=post$a + post$b*weight ,
    sd=post$sigma ) )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

## R code 4.64
library(rethinking)
data(Howell1)
d <- Howell1
str(d)

## R code 4.65
d$weight.s <- ( d$weight - mean(d$weight) )/sd(d$weight)

## R code 4.66
d$weight.s2 <- d$weight.s^2
m4.5 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight.s + b2*weight.s2 ,
    a ~ dnorm( 178 , 100 ) ,
    b1 ~ dnorm( 0 , 10 ) ,
    b2 ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d )

## R code 4.67
precis( m4.5 )

## R code 4.68
weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
pred_dat <- list( weight.s=weight.seq , weight.s2=weight.seq^2 )
mu <- link( m4.5 , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.5 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

## R code 4.69
plot( height ~ weight.s , d , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )

## R code 4.70
d$weight.s3 <- d$weight.s^3
m4.6 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight.s + b2*weight.s2 + b3*weight.s3 ,
    a ~ dnorm( 178 , 100 ) ,
    b1 ~ dnorm( 0 , 10 ) ,
    b2 ~ dnorm( 0 , 10 ) ,
    b3 ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d )

## R code 4.71
plot( height ~ weight.s , d , col=col.alpha(rangi2,0.5) , xaxt="n" )

## transform data from z-scores back to original scale
at <- c(-2,-1,0,1,2)
labels <- at*sd(d$weight) + mean(d$weight)
axis( side=1 , at=at , labels=round(labels,1) )

## R code 4.73
plot( height ~ weight , data=Howell1 ,
      col=col.alpha(rangi2,0.4) )
