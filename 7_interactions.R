library(rethinking)
data(rugged)
d <- rugged

# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )

# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]

# split countries into Africa and not-Africa
d.A1 <- dd[ dd$cont_africa==1 , ] # Africa
d.A0 <- dd[ dd$cont_africa==0 , ] # not Africa

m7.3 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged ,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=dd )
# dummy column for africa nation
m7.4 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa ,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=dd )

compare( m7.3 , m7.4 )

# compre slope on african vs. non-african

d.A0.samples <- link(m7.4, data = d.A0)
d.A1.samples <- link(m7.4, data = d.A1)
par(mfrow = c(1,2))
plot(log_gdp ~ rugged, data = d.A0, main = 'Not Africa')
plot(log_gdp ~ rugged, data = d.A1, main = 'Africa')

ci_pi_plot <- function(model, data, prob, title, africa_ind){
  plot(log_gdp ~ rugged, d , col=rangi2, main = title)
  rugged.seq <- seq(0, 7, by = 0.01)
  preds <- link(model, data=data.frame(rugged = rugged.seq,
                                       cont_africa = africa_ind))
  mu <- apply(preds, 2, mean)
  # prediction interval for mu
  mu.PI <- apply(preds, 2, PI, prob = prob)
  # confidence interval for target height - simulate posterior observations
  h <- sim(model, data = data.frame(rugged = rugged.seq,
                                    cont_africa = africa_ind))
  h.CI <- apply(h, 2, PI, prob = prob)
  # plot it all
  lines(rugged.seq, mu, lty=2)
  shade(mu.PI, rugged.seq)
  shade(h.CI, rugged.seq)
}
# predicted means are shifted, but same slope
ci_pi_plot(m7.5, d.A0, 0.97, 'Not Africa', 0)
ci_pi_plot(m7.5, d.A1, 0.97, 'Africa', 1)

# interaction model
m7.5 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + gamma*rugged + bA*cont_africa ,
    gamma <- bR + bAR*cont_africa ,
    a ~ dnorm( 8 , 100 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bAR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=dd )
# now predicted means have different slopes!
ci_pi_plot(m7.5, d.A0, 0.97, 'Not Africa', 0)
ci_pi_plot(m7.5, d.A1, 0.97, 'Africa', 1)

# posterior of interactions, Africa vs. not Africa
post <- extract.samples( m7.5 )
gamma.Africa <- post$bR + post$bAR*1
gamma.notAfrica <- post$bR + post$bAR*0
# overlay posteriors
par(mfrow = c(1,1))
dens( gamma.Africa , xlim=c(-0.5,0.6) , ylim=c(0,5.5) ,
      xlab="gamma" , col=rangi2 )
dens( gamma.notAfrica , add=TRUE )

# plot symmetrical definition of interaction
# get minimum and maximum rugged values
q.rugged <- range(dd$rugged)

# compute lines and confidence intervals
mu.ruggedlo <- link( m7.5 ,
                     data=data.frame(rugged=q.rugged[1],cont_africa=0:1) )
mu.ruggedlo.mean <- apply( mu.ruggedlo , 2 , mean )
mu.ruggedlo.PI <- apply( mu.ruggedlo , 2 , PI )

mu.ruggedhi <- link( m7.5 ,
                     data=data.frame(rugged=q.rugged[2],cont_africa=0:1) )
mu.ruggedhi.mean <- apply( mu.ruggedhi , 2 , mean )
mu.ruggedhi.PI <- apply( mu.ruggedhi , 2 , PI )

# plot it all, splitting points at median
med.r <- median(dd$rugged)
ox <- ifelse( dd$rugged > med.r , 0.05 , -0.05 )
plot( dd$cont_africa + ox , log(dd$rgdppc_2000) ,
      col=ifelse(dd$rugged>med.r,rangi2,"black") ,
      xlim=c(-0.25,1.25) , xaxt="n" , ylab="log GDP year 2000" ,
      xlab="Continent" )
axis( 1 , at=c(0,1) , labels=c("other","Africa") )
lines( 0:1 , mu.ruggedlo.mean , lty=2 )
shade( mu.ruggedlo.PI , 0:1 )
lines( 0:1 , mu.ruggedhi.mean , col=rangi2 )
shade( mu.ruggedhi.PI , 0:1 , col=col.alpha(rangi2,0.25) )

# tulips data - continuous interactions

library(rethinking)
data(tulips)
d <- tulips
str(d)
# won't converge! need to standardize or change algo...
m7.6 <- map(
  alist(
    blooms ~ dnorm( mu , sigma ) ,
    mu <- a + bW*water + bS*shade ,
    a ~ dnorm( 0 , 100 ) ,
    bW ~ dnorm( 0 , 100 ) ,
    bS ~ dnorm( 0 , 100 ) ,
    sigma ~ dunif( 0 , 100 )
  ) ,
  data=d )
m7.7 <- map(
  alist(
    blooms ~ dnorm( mu , sigma ) ,
    mu <- a + bW*water + bS*shade + bWS*water*shade ,
    a ~ dnorm( 0 , 100 ) ,
    bW ~ dnorm( 0 , 100 ) ,
    bS ~ dnorm( 0 , 100 ) ,
    bWS ~ dnorm( 0 , 100 ) ,
    sigma ~ dunif( 0 , 100 )
  ) ,
  data=d )
# solve convergence issues with Nelder-Mead
m7.6 <- map(
  alist(
    blooms ~ dnorm( mu , sigma ) ,
    mu <- a + bW*water + bS*shade ,
    a ~ dnorm( 0 , 100 ) ,
    bW ~ dnorm( 0 , 100 ) ,
    bS ~ dnorm( 0 , 100 ) ,
    sigma ~ dunif( 0 , 100 )
  ) ,
  data=d ,
  method="Nelder-Mead" ,
  control=list(maxit=1e4) )
m7.7 <- map(
  alist(
    blooms ~ dnorm( mu , sigma ) ,
    mu <- a + bW*water + bS*shade + bWS*water*shade ,
    a ~ dnorm( 0 , 100 ) ,
    bW ~ dnorm( 0 , 100 ) ,
    bS ~ dnorm( 0 , 100 ) ,
    bWS ~ dnorm( 0 , 100 ) ,
    sigma ~ dunif( 0 , 100 )
  ) ,
  data=d ,
  method="Nelder-Mead" ,
  control=list(maxit=1e4) )

coeftab(m7.6,m7.7)
compare( m7.6 , m7.7 )
# tryptich plots on centered model
d$shade.c <- d$shade - mean(d$shade)
d$water.c <- d$water - mean(d$water)

m7.9 <- map(
  alist(
    blooms ~ dnorm( mu , sigma ) ,
    mu <- a + bW*water.c + bS*shade.c + bWS*water.c*shade.c ,
    a ~ dnorm( 130 , 100 ) ,
    bW ~ dnorm( 0 , 100 ) ,
    bS ~ dnorm( 0 , 100 ) ,
    bWS ~ dnorm( 0 , 100 ) ,
    sigma ~ dunif( 0 , 100 )
  ) ,
  data=d ,
  start=list(a=mean(d$blooms),bW=0,bS=0,bWS=0,sigma=sd(d$blooms)) )

plot(blooms ~ shade.c, d)
shade.seq <- seq(-1, 1, .01)
water.1 <- link(m7.9, data = data.frame(water.c = -1, shade.c = shade.seq))
water.2 <- link(m7.9, data = data.frame(water.c = 0, shade.c = shade.seq))
water.3 <- link(m7.9, data = data.frame(water.c = 1, shade.c = shade.seq))

water.1.mu <- apply(water.1, 2, mean)
water.2.mu <- apply(water.2, 2, mean)
water.3.mu <- apply(water.3, 2, mean)
par(mfrow = c(1,3))
plot(x = shade.seq, y = water.1.mu, ylim = c(0, 370), main = 'Low Water')
plot(x = shade.seq, y = water.2.mu, ylim = c(0, 370), main = 'Average Water')
plot(x = shade.seq, y = water.3.mu, ylim = c(0, 370), main = 'High Water')
