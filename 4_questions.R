library(rethinking)

#4M1
mu.s <- rnorm(1e4, 0, 10)
sigma.s <- runif(1e4, 0, 10)

samples <- rnorm(1e4, mu.s, sigma.s)

#4M2
?map
flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm(0, 10) ,
  sigma ~ dunif(0, 10)
)
map_val <- map(flist)

# !Kung San!
data(Howell1)
d <- Howell1
# height is correlated with age, before adulthood
d2 <- d[ d$age >= 18 , ]
weight <- d2$weight
h1 <- map(alist(
  height ~ dnorm(mu , sigma),
  mu <- a + b*weight,
  a ~ dnorm(0, 100),
  b ~ dnorm(0, 10),
  sigma ~ dunif(0, 50)
  ),
  data = d2)

new_data <- data.frame(id = seq(1:5),
                       weight = c(46.95, 43.72, 64.78, 32.59, 54.63))
# generate posterior distribution of height
# for supplied values of weight
mu <- link(h1, data=data.frame(weight=new_data$weight))
# 90% HPDI of height for each value of weight 
# intervals for average predicted height, not actual height
apply(mu, 2, mean)
apply(mu, 2, HPDI, 0.9)

# prediction interval for actual heights
m <- sim(h1, data = list(weight = new_data$weight))
apply(m, 2, mean)
# interval of possible observable heights
# for given weights
apply(m, 2, PI, 0.9)

#4H2
# filter to youths only
d3 <- d[d$age < 18,]

# fit linear model
h2 <- map(alist(
  height ~ dnorm(mu , sigma),
  mu <- a + b*weight,
  a ~ dnorm(0, 100),
  b ~ dnorm(0, 10),
  sigma ~ dunif(0, 50)
),
data = d3)
precis(h2)
10*coef(h1)['b']
# 27.20458
plot(d3$height ~ d3$weight)
abline(a = coef(h2)['a'], b = coef(h2)['b'])

# generate posterior for predicted means
weight.seq <- seq(0, 40, 1)
mu <- link(h2, data = list(weight = weight.seq))
# hpdi for predicted means
mu.hpdi <- apply(mu, 2, HPDI, .89)
shade(mu.hpdi, weight.seq)

# observed heights HPDI
m <- sim(fit = h2, data = list(weight = weight.seq))
m.pi <- apply(m, 2, HPDI, .89)
shade(m.pi, weight.seq)

#4H3
# fit linear model with predictor log(weight)
h3 <- map(alist(
  height ~ dnorm(mu , sigma),
  mu <- a + b*log(weight),
  a ~ dnorm(178, 100),
  b ~ dnorm(0, 100),
  sigma ~ dunif(0, 50)
),
data = d)

precis(h3)

plot(height ~ weight, data = Howell1, col = col.alpha(rangi2, 0.4))

# generate posterior for predicted means
weight.seq <- seq(0, 60, 1)
mu <- link(h3, data = list(weight = weight.seq))
# hpdi for predicted means
mu.mean <- apply(mu, 2, mean)
lines(weight.seq,mu.mean)
mu.hpdi <- apply(mu, 2, HPDI, .97)
shade(mu.hpdi, weight.seq)

# observed heights HPDI
m <- sim(fit = h3, data = list(weight = weight.seq))
m.pi <- apply(m, 2, HPDI, .97)
shade(m.pi, weight.seq)

