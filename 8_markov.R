library(rethinking)
# King markov
num_weeks <- 1e5
positions <- rep(0,num_weeks)
current <- 10
for ( i in 1:num_weeks ) {
  # record current position
  positions[i] <- current
  
  # flip coin to generate proposal
  proposal <- current + sample( c(-1,1) , size=1 )
  # now make sure he loops around the archipelago
  if ( proposal < 1 ) proposal <- 10
  if ( proposal > 10 ) proposal <- 1
  
  # move?
  prob_move <- proposal/current
  current <- ifelse( runif(1) < prob_move , proposal , current )
}
# show the path
par(mfrow=c(1,2))
plot(x = 1:500, y = positions[1:500], xlab = 'weeks', ylab = 'island')
plot(table(positions), xlab = 'island', ylab = 'visits')

# rugged data, using stan's HMC
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]

# old method of fitting model:
m8.1 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ) ,
  data=dd )
precis(m8.1)

# clean dataframe for stan
dd.trim <- dd[ , c("log_gdp","rugged","cont_africa") ]
str(dd.trim)

# stan model
m8.1stan <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ) ,
  data=dd.trim )

precis(m8.1stan)
precis(m8.1)

# draw samples from model, parallelizing the chains
m8.1stan_4chains <- map2stan( m8.1stan , chains=4 , cores=4 )
precis(m8.1stan_4chains)
m8.1stan_4chains


# extract samples from stan model
post <- extract.samples(m8.1stan)
str(post)
pairs(post, col = rangi2)
show(m8.1stan)

# show trace plot
plot(m8.1stan)

# build a bad wild chain!
y <- c(-1,1)
m8.2 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- alpha
  ) ,
  data=list(y=y) , start=list(alpha=0,sigma=1) ,
  chains=2 , iter=4000 , warmup=1000 )

## R code 8.14
precis(m8.2)
plot(m8.2)

# fix chains by using weak priors
m8.3 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- alpha ,
    alpha ~ dnorm( 1 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ) ,
  data=list(y=y) , start=list(alpha=0,sigma=1) ,
  chains=2 , iter=4000 , warmup=1000 )
precis(m8.3)

# cauchy will never converge!
y <- rcauchy(1e4,0,5)
mu <- sapply( 1:length(y) , function(i) sum(y[1:i])/i )
plot(mu,type="l")


y <- rnorm( 100 , mean=0 , sd=1 )

m8.4 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    sigma ~ dcauchy( 0 , 1 )
  ) ,
  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) ,
  chains=2 , iter=4000 , warmup=1000 )
precis(m8.4)
