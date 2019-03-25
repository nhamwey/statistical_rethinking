library(rethinking)
data(tulips)
d <- tulips
# 7H1
# create dummy columns for bed
d$bed.a <- ifelse(d$bed == 'a', 1, 0)
d$bed.b <- ifelse(d$bed == 'b', 1, 0)
d$bed.c <- ifelse(d$bed == 'c', 1, 0)
# use centering
d$shade.c <- d$shade - mean(d$shade)
d$water.c <- d$water - mean(d$water)

m7H1 <- map(
  alist(
    blooms ~ dnorm( mu , sigma ) ,
    mu <- a + bW*water.c + bS*shade.c + bBa*bed.a + bBb*bed.b,
    a ~ dnorm( 130 , 100 ) ,
    bW ~ dnorm( 0 , 100 ) ,
    bS ~ dnorm( 0 , 100 ) ,
    bBa ~ dnorm( 0 , 100 ) ,
    bBb ~ dnorm( 0 , 100 ) ,
    sigma ~ dunif( 0 , 100 )
  ) ,
  data=d ,
  start=list(a=mean(d$blooms),bW=0,bS=0,bBa=0,bBb=0,sigma=sd(d$blooms)) )

precis(m7H1)

# 7H2
# model which omits bed
m7H2 <- map(
  alist(
    blooms ~ dnorm( mu , sigma ) ,
    mu <- a + bW*water.c + bS*shade.c,
    a ~ dnorm( 130 , 100 ) ,
    bW ~ dnorm( 0 , 100 ) ,
    bS ~ dnorm( 0 , 100 ) ,
    sigma ~ dunif( 0 , 100 )
  ) ,
  data=d ,
  start=list(a=mean(d$blooms),bW=0,bS=0,sigma=sd(d$blooms)) )

compare(m7H1, m7H2)
# bed model is slightly better... but not by much
# solution says to look at posterior of distributional differences
# using coerce_index to fit separate models for each level

#7H3
data(rugged)
d <- rugged

# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )

# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]

# split countries into Africa and not-Africa
d.A1 <- dd[ dd$cont_africa==1 , ] # Africa
d.A0 <- dd[ dd$cont_africa==0 , ] # not Africa

m7H3 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    bAR ~ dnorm(0, 1),
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=dd )

dd.xs <- dd[dd$country != 'Seychelles',]

m7H3_xsey <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    bAR ~ dnorm(0, 1),
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=dd.xs)

d.A0.samples <- link(m7H3, data = d.A0)
d.A1.samples <- link(m7H3, data = d.A1)
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
par(mfrow = c(2,2))
ci_pi_plot(m7H3, d.A0, 0.97, 'Not Africa', 0)
ci_pi_plot(m7H3, d.A1, 0.97, 'Africa', 1)
ci_pi_plot(m7H3_xsey, d.A0, 0.97, 'Not Africa', 0)
ci_pi_plot(m7H3_xsey, d.A1, 0.97, 'Africa', 1)
coeftab(m7H3, m7H3_xsey)

m7H3_xsey1 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=dd.xs)

m7H3_xsey2 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=dd.xs)

m7H3_xsey3 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    bAR ~ dnorm(0, 1),
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=dd.xs)
par(mfrow = c(3,2))
# I was supposed to make an ensemble model, but oh well
ci_pi_plot(m7H3_xsey1, d.A0, 0.97, 'Not Africa', 0)
ci_pi_plot(m7H3_xsey1, d.A1, 0.97, 'Africa', 1)
ci_pi_plot(m7H3_xsey2, d.A0, 0.97, 'Not Africa', 0)
ci_pi_plot(m7H3_xsey2, d.A1, 0.97, 'Africa', 1)
ci_pi_plot(m7H3_xsey3, d.A0, 0.97, 'Not Africa', 0)
ci_pi_plot(m7H3_xsey3, d.A1, 0.97, 'Africa', 1)

coeftab(m7H3_xsey1, m7H3_xsey2, m7H3_xsey3)

# 7H4
data("nettle")
d <- nettle
d$lang.per.cap <- d$num.lang / d$k.pop
d$log.lang <- log(d$lang.per.cap)
# a) covariates mean.growing.season & log(area)
m7H42 <- map(
  alist(
    log.lang ~ dnorm(mu , sigma) ,
    mu <- a + bMGS*mean.growing.season,
    a ~ dnorm( 8 , 100 ) ,
    bMGS ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d)

m7H4A3 <- map(
  alist(
    log.lang ~ dnorm(mu , sigma) ,
    mu <- a + bMGS*mean.growing.season + bLA*log(area),
    a ~ dnorm( 8 , 100 ) ,
    bMGS ~ dnorm( 0 , 1 ) ,
    bLA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d)
par(mfrow = c(1,1))
plot(log.lang ~ mean.growing.season, d)
ci_pi_plot <- function(model, data, prob, title){
  plot(log.lang ~ mean.growing.season, d , col=rangi2, main = title)
  season.seq <- seq(0, 12, by = 0.01)
  preds <- link(model, data=data.frame(mean.growing.season = season.seq,
                                       area = mean(d$area)))
  mu <- apply(preds, 2, mean)
  # prediction interval for mu
  mu.PI <- apply(preds, 2, PI, prob = prob)
  # confidence interval for target height - simulate posterior observations
  h <- sim(model, data = data.frame(mean.growing.season = season.seq,
                                    area = mean(d$area)))
  h.CI <- apply(h, 2, PI, prob = prob)
  # plot it all
  lines(season.seq, mu, lty=2)
  shade(mu.PI, season.seq)
  shade(h.CI, season.seq)
}
# test if log(lang) has positive relationship with mean.growing.season
par(mfrow = c(2,2))
ci_pi_plot(m7H4A, d, 0.97, 'Basic Model')
coeftab(m7H4A)

# b)
# model with intercept only
m7H4B1 <- map(
  alist(
    log.lang ~ dnorm(mu , sigma) ,
    mu <- a,
    a ~ dnorm( 8 , 100 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d)
# model with sd growing season
m7H4B2 <- map(
  alist(
    log.lang ~ dnorm(mu , sigma) ,
    mu <- a + bSGS*sd.growing.season,
    a ~ dnorm( 8 , 100 ) ,
    bSGS ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d)
# model with sd growing season and log(area)
m7H4B3 <- map(
  alist(
    log.lang ~ dnorm(mu , sigma) ,
    mu <- a + bSGS*sd.growing.season + bLA*log(area),
    a ~ dnorm( 8 , 100 ) ,
    bSGS ~ dnorm( 0 , 1 ) ,
    bLA ~ dnorm(0, 1),
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d)

ci_pi_plot <- function(model, data, prob, title){
  plot(log.lang ~ sd.growing.season, d , col=rangi2, main = title)
  season.seq <- seq(0, 6, by = 0.01)
  preds <- link(model, data=data.frame(sd.growing.season = season.seq,
                                       area = mean(d$area)))
  mu <- apply(preds, 2, mean)
  # prediction interval for mu
  mu.PI <- apply(preds, 2, PI, prob = prob)
  # confidence interval for target height - simulate posterior observations
  h <- sim(model, data = data.frame(sd.growing.season = season.seq,
                                    area = mean(d$area)))
  h.CI <- apply(h, 2, PI, prob = prob)
  # plot it all
  lines(season.seq, mu, lty=2)
  shade(mu.PI, season.seq)
  shade(h.CI, season.seq)
}
par(mfrow = c(1,1))
ci_pi_plot(m7H4B2, d, .97, 'SD Growing Season')
compare(m7H4B1, m7H4B2, m7H4B3)

# c)

m7H4C <- map(
  alist(
    log.lang ~ dnorm(mu , sigma) ,
    mu <- a + bMGS*mean.growing.season + bSGS*sd.growing.season +
          bINT*mean.growing.season*sd.growing.season,
    a ~ dnorm( 8 , 100 ) ,
    c(bSGS, bMGS, bINT) ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d)
compare(m7H4B2, m7H42, m7H4C, m7H4B1)

trip.low = min(d$sd.growing.season)
trip.mid = median(d$sd.growing.season)
trip.high = max(d$sd.growing.season)
mgs.seq = seq(0, 12, by = 1)

int_link.low <- link(m7H4C, data = data.frame(sd.growing.season = trip.low,
                                              mean.growing.season = mgs.seq))
int_link.mid <- link(m7H4C, data = data.frame(sd.growing.season = trip.mid,
                                              mean.growing.season = mgs.seq))
int_link.high <- link(m7H4C, data = data.frame(sd.growing.season = trip.high,
                                               mean.growing.season = mgs.seq))
int.low.mean <- apply(int_link.low, 2, mean)
int.mid.mean <- apply(int_link.mid, 2, mean)
int.high.mean <- apply(int_link.high, 2, mean)
par(mfrow = c(1,3))
plot(x = mgs.seq, y = int.low.mean)
plot(x = mgs.seq, y = int.mid.mean)
plot(x = mgs.seq, y = int.high.mean)
coeftab(m7H4C)
