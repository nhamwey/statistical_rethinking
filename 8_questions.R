library(rethinking)

#8M1
# rugged data, using stan's HMC
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]

# clean dataframe for stan
dd.trim <- dd[ , c("log_gdp","rugged","cont_africa") ]
str(dd.trim)

# stan model
m81astan <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ) ,
  data=dd.trim )

m81bstan <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dexp(1)
  ) ,
  data=dd.trim )
# no noticeable difference for parameter estimates
precis(m81astan)
precis(m81bstan)
# different priors
curve(dcauchy(x, 0, 2), from = 0, to = 10, ylim = c(0,1))
curve(dexp(x, 1), add = T, col = 'red')
curve(dunif(x, 0, 10), add = T, col = 'blue')
# same posterior
sigma_unif <- extract.samples(m81astan, pars = 'sigma')
sigma_exp <- extract.samples(m81bstan, pars = 'sigma')
dens(sigma_unif[[1]])
dens(sigma_exp[[1]], add = T, col = 'blue')

#8M2
# try exp with mean = 1/10
m82stan <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dexp(10)
  ) ,
  data=dd.trim )
# exp with mean = 1/100
m82stan_100 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dexp(100)
  ) ,
  data=dd.trim )
# drags posterior towars zero!
sigma_exp <- extract.samples(m81bstan, pars = 'sigma')
sigma_exp10 <- extract.samples(m82stan, pars = 'sigma')
sigma_exp100 <- extract.samples(m82stan_100, pars = 'sigma')
dens(sigma_exp100[[1]], xlim = c(0.6, 1.1))
dens(sigma_exp10[[1]], add = T, col = rainbow(2))
dens(sigma_exp[[1]], add = T, col = rainbow(100)[25])

#8M3
# stan model
m8.1_100stan <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ) ,
  data=dd.trim,
  chains=1 , iter=3100 , warmup=100 )
m8.1_300stan <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ) ,
  data=dd.trim,
  chains=1 , iter=3300 , warmup=300 )
m8.1_500stan <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ) ,
  data=dd.trim,
  chains=1, iter=3500 , warmup=500 )
# very small number of warmup iterations needed...
plot(m8.1_100stan)
plot(m8.1_300stan)
plot(m8.1_500stan)

#8H1
mp <- map2stan(
  alist(
    a ~ dnorm(0,1),
    b ~ dcauchy(0,1)
  ),
  data=list(y=1),
  start=list(a=0,b=0),
  iter=1e4, warmup=100 , WAIC=FALSE )
plot(mp)
# results in sampling from priors only... no likelihood!
# normal oscillates around zero, but cauchy has fat tail, occassionally gets a whacky value
# trace for cauchy is normal - it needs to sometimes sample far out...
mp_samples <- extract.samples(mp)
dens(mp_samples[['a']])
dens(mp_samples[['b']])

#8H2
# refit divorce models from chapter 5
data(WaffleDivorce)
d <- WaffleDivorce
# model predicting divorce, using median marriage age as a predictor
# standardize predictor
d$MedianAgeMarriage_s <- (d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/
  sd(d$MedianAgeMarriage)
d_1 <- d[,c('Divorce', 'MedianAgeMarriage_s')]
m5.1 <- map2stan(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bA * MedianAgeMarriage_s ,
    a ~ dnorm( 10 , 10 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data = d_1)
plot(m5.1)
d$Marriage_s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)
d_2 <- d[,c('Divorce', 'Marriage_s')]
m5.2 <- map2stan(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bR * Marriage_s ,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data = d_2)
d_3 <- d[,c('Divorce', 'MedianAgeMarriage_s', 'Marriage_s')]
## R code 5.4
m5.3 <- map2stan(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bR*Marriage_s + bA*MedianAgeMarriage_s ,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data = d_3)
# marriage rate not useful predictor, WAIC ranks models with median age as nearly equal
compare(m5.1, m5.2, m5.3)
plot(m5.3)
plot(m5.2)
plot(m5.1)
precis(m5.3)
precis(m5.2)
precis(m5.1)

#8H3
N <- 100                          # number of individuals
height <- rnorm(N,10,2)           # sim total height of each
leg_prop <- runif(N,0.4,0.5)      # leg as proportion of height
leg_left <- leg_prop*height +     # sim left leg as proportion + error
  rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height +    # sim right leg as proportion + error
  rnorm( N , 0 , 0.02 )
# combine into data frame
d <- data.frame(height,leg_left,leg_right)


m5.8s <- map2stan(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ) ,
  data=d, chains=4,
  start=list(a=10,bl=0,br=0,sigma=1))
# truncate normal prior for br to be stricly positive using T[0,]
m5.8s2 <- map2stan(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) & T[0,] ,
    sigma ~ dcauchy( 0 , 1 )
  ) ,
  data=d, chains=4,
  start=list(a=10,bl=0,br=0,sigma=1) )
# truncated model has difficulty converginG
# bl and br are perfectly negatively correlated, since their input distributions
# are perfectly positvely correlated
# when you truncate one of them, it cascades to the other posterior too (in order to maintain correlation)
plot(m5.8s)
plot(m5.8s2)
precis(m5.8s)
precis(m5.8s2)
pairs(m5.8s)
pairs(m5.8s2)

#8H4
compare(m5.8s, m5.8s2)
precis(m5.8s)
precis(m5.8s2)

#8H5
num_weeks <- 1e5
positions <- rep(0,num_weeks)
current <- 10
island_dist <- c(11, 2, 33, 4, 5, 66, 7, 8, 9, 10)
names(island_dist) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
for (i in 1:num_weeks) {
  # record current position
  positions[i] <- current
  
  # flip coin to generate proposal
  proposal <- current + sample( c(-1,1) , size=1 )
  # now make sure he loops around the archipelago
  if ( proposal < 1 ) proposal <- 10
  if ( proposal > 10 ) proposal <- 1
  
  # move?
  prob_move <- island_dist[proposal]/island_dist[current]
  current <- ifelse( runif(1) < prob_move , proposal , current )
}
# show the path
par(mfrow=c(1,2))
plot(x = 1:500, y = positions[1:500], xlab = 'weeks', ylab = 'island')
plot(table(positions), xlab = 'island', ylab = 'visits')

#8M6
# build metropolis algorithm for globe tossing example
num_samples <- 1e4
p_samples <- rep(NA,num_samples)
p <- 0.5 # initialize chain with p=0.5
for ( i in 1:num_samples ) {
  # record current parameter value
  p_samples[i] <- p
  
  # generate a uniform proposal from -0.1 to +0.1
  proposal <- p + runif(1,-0.1,0.1)
  # now reflect off boundaries at 0 and 1
  # this is needed so proposals are symmetric
  if ( proposal < 0 ) proposal <- abs(proposal)
  if ( proposal > 1 ) proposal <- 1-(proposal-1)
  
  # compute posterior prob of current and proposal
  prob_current <- dbinom(6,size=9,prob=p) * dunif(p,0,1)
  prob_proposal <- dbinom(6,size=9,prob=proposal) * dunif(proposal,0,1)
  
  # move?
  prob_move <- prob_proposal/prob_current
  p <- ifelse( runif(1) < prob_move , proposal , p )
}

## R code 7.22
plot( p_samples , type="l" , ylab="probability water" )

## R code 7.23
dens( p_samples , xlab="probability water" )
curve( dbeta(x,7,4) , add=TRUE , col="red" )
