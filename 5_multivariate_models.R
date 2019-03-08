library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
# model predicting divorce, using median marriage age as a predictor
# standardize predictor
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/
  sd(d$MedianAgeMarriage)

# fit model
m5.1 <- map(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bA * MedianAgeMarriage.s ,
    a ~ dnorm( 10 , 10 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data = d )

MAM.seq <- seq( from=-3 , to=3.5 , length.out=30 )
mu <- link( m5.1 , data=data.frame(MedianAgeMarriage.s=MAM.seq) )
mu.PI <- apply( mu , 2 , PI )

# plot it all
plot( Divorce ~ MedianAgeMarriage.s , data=d , col=rangi2 )
abline( m5.1 )
shade( mu.PI , MAM.seq )

# predict divorce using marriage rate
d$Marriage.s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)
m5.2 <- map(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bR * Marriage.s ,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data = d )

## R code 5.4
m5.3 <- map(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s ,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data = d )
precis( m5.3 )

## R code 5.5
plot( precis(m5.3) )

# residual predictor plots
# predict marriage rate using median marriage age

m5.4 <- map(
  alist(
    Marriage.s ~ dnorm( mu , sigma ) ,
    mu <- a + b*MedianAgeMarriage.s ,
    a ~ dnorm( 0 , 10 ) ,
    b ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data = d )

# compute expected value at MAP, for each State
mu <- coef(m5.4)['a'] + coef(m5.4)['b']*d$MedianAgeMarriage.s
# compute residual for each State
m.resid <- d$Marriage.s - mu

plot( Marriage.s ~ MedianAgeMarriage.s , d , col=rangi2 )
abline( m5.4 )
# loop over States
for ( i in 1:length(m.resid) ) {
  x <- d$MedianAgeMarriage.s[i] # x location of line segment
  y <- d$Marriage.s[i] # observed endpoint of line segment
  # draw the line segment
  lines( c(x,x) , c(mu[i],y) , lwd=0.5 , col=col.alpha("black",0.7) )
}

plot(d$Divorce ~ m.resid)

# prepare new counterfactual data
A.avg <- mean( d$MedianAgeMarriage.s )
R.seq <- seq( from=-3 , to=3 , length.out=30 )
pred.data <- data.frame(
  Marriage.s=R.seq,
  MedianAgeMarriage.s=A.avg
)

# compute counterfactual mean divorce (mu)
mu <- link( m5.3 , data=pred.data )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )

# simulate counterfactual divorce outcomes
R.sim <- sim( m5.3 , data=pred.data , n=1e4 )
R.PI <- apply( R.sim , 2 , PI )

# display predictions, hiding raw data with type="n"
plot( Divorce ~ Marriage.s , data=d , type="n" )
mtext( "MedianAgeMarriage.s = 0" )
lines( R.seq , mu.mean )
shade( mu.PI , R.seq )
shade( R.PI , R.seq )

# posterior prediction plots
mu <- link( m5.3 )

# summarize samples across cases
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )

# simulate observations
# again no new data, so uses original data
divorce.sim <- sim( m5.3 , n=1e4 )
divorce.PI <- apply( divorce.sim , 2 , PI )

## R code 5.12
plot( mu.mean ~ d$Divorce , col=rangi2 , ylim=range(mu.PI) ,
      xlab="Observed divorce" , ylab="Predicted divorce" )
abline( a=0 , b=1 , lty=2 )
for ( i in 1:nrow(d) )
  lines( rep(d$Divorce[i],2) , c(mu.PI[1,i],mu.PI[2,i]) ,
         col=rangi2)

## R code 5.13
identify( x=d$Divorce , y=mu.mean , labels=d$Loc , cex=0.8 )

# residual charts
## R code 5.14
# compute residuals
divorce.resid <- d$Divorce - mu.mean
# get ordering by divorce rate
o <- order(divorce.resid)
# make the plot
dotchart( divorce.resid[o] , labels=d$Loc[o] , xlim=c(-6,5) , cex=0.6 )
abline( v=0 , col=col.alpha("black",0.2) )
for ( i in 1:nrow(d) ) {
  j <- o[i] # which State in order
  # plot mu prediction interval
  lines( d$Divorce[j]-c(mu.PI[1,j],mu.PI[2,j]) , rep(i,2) )
  # plot observed divorce interval
  points( d$Divorce[j]-c(divorce.PI[1,j],divorce.PI[2,j]) , rep(i,2),
          pch=3 , cex=0.6 , col="gray" )
}

plot(d$WaffleHouses/d$Population, divorce.resid)

## R code 5.15
N <- 100                         # number of cases
x_real <- rnorm( N )             # x_real as Gaussian with mean 0 and stddev 1
x_spur <- rnorm( N , x_real )    # x_spur as Gaussian with mean=x_real
y <- rnorm( N , x_real )         # y as Gaussian with mean=x_real
d <- data.frame(y,x_real,x_spur) # bind all together in data frame

head(d)
spur <- lm(y~x_spur, d)
spur <- lm(y~x_real, d)
plot(spur)


library(rethinking)
data(milk)
d <- milk
str(d)

d$neocortex.perc

dcc <- d[!is.na(d$neocortex.perc),]
m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bn*neocortex.perc ,
    a ~ dnorm( 0 , 100 ) ,
    bn ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 1 )
  ) ,
  data=dcc )

precis(m5.5, digits = 3)

np.seq <- 0:100
pred.data <- data.frame( neocortex.perc=np.seq )

mu <- link( m5.5 , data=pred.data , n=1e4 )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )

plot( kcal.per.g ~ neocortex.perc , data=dcc , col=rangi2 )
lines( np.seq , mu.mean )
lines( np.seq , mu.PI[1,] , lty=2 )
lines( np.seq , mu.PI[2,] , lty=2 )

dcc$log.mass <- log(dcc$mass)
m5.6 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bn*log.mass ,
    a ~ dnorm( 0 , 100 ) ,
    bn ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 1 )
  ) ,
  data=dcc )

pred.data <- data.frame( log.mass=seq(-2.5, 5, .01))

mu <- link( m5.6 , data=pred.data , n=1e4 )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )

plot( kcal.per.g ~ log.mass , data=dcc , col=rangi2 )
lines( pred.data$log.mass , mu.mean )
lines( pred.data$log.mass , mu.PI[1,] , lty=2 )
lines( pred.data$log.mass , mu.PI[2,] , lty=2 )


# positive correlation and negative
N <- 10000                         # number of cases
rho <- 0.7                       # correlation btw x_pos and x_neg
x_pos <- rnorm( N )              # x_pos as Gaussian
x_neg <- rnorm( N , rho*x_pos ,  # x_neg correlated with x_pos
                sqrt(1-rho^2) )
y <- rnorm( N , x_pos - x_neg )  # y equally associated with x_pos, x_neg
d <- data.frame(y,x_pos,x_neg)   # bind all together in data frame
pairs(d)

#multicollinearity
N <- 100                          # number of individuals
height <- rnorm(N,10,2)           # sim total height of each
leg_prop <- runif(N,0.4,0.5)      # leg as proportion of height
leg_left <- leg_prop*height +     # sim left leg as proportion + error
  rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height +    # sim right leg as proportion + error
  rnorm( N , 0 , 0.02 )
# combine into data frame
d <- data.frame(height,leg_left,leg_right)

m5.8 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d )
plot(precis(m5.8))
par(mfrow = c(1,1))
# sample from posterior... coefficient estimates are highly correlated
post <- extract.samples(m5.8)
plot( bl ~ br , post , col=col.alpha(rangi2,0.1) , pch=16 )
# posterior mean is correct... but highly wild parameters
sum_blbr <- post$bl + post$br
dens( sum_blbr , col=rangi2 , lwd=2 , xlab="sum of bl and br" )

# milk
library(rethinking)
data(milk)
d <- milk

## R code 5.36
# kcal.per.g regressed on perc.fat
m5.10 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bf*perc.fat ,
    a ~ dnorm( 0.6 , 10 ) ,
    bf ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d )


# intercept for each level
# used in multilevel models
( d$clade_id <- coerce_index(d$clade) )


m5.16_alt <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a[clade_id] ,
    a[clade_id] ~ dnorm( 0.6 , 10 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d )
precis( m5.16_alt , depth=2 )

## R code 5.56
m5.17 <- lm( y ~ 1 + x , data=d )
m5.18 <- lm( y ~ 1 + x + z + w , data=d )



