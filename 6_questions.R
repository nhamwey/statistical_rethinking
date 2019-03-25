library(rethinking)
#6E2
p <- c(0.3, 0.7)

entropy <- function(p){
  p*log(p)
}
-sum(sapply(p, entropy))

#6E3
p3 <- c(.2, .25, .25, .3)
-sum(sapply(p3, entropy))

#6E3
p4 <- c(.33, .33, .33)
-sum(sapply(p4, entropy))

#6H1
library(rethinking)
data(Howell1)
d <- Howell1
d$age <- (d$age - mean(d$age))/sd(d$age)
set.seed( 1000 )
i <- sample(1:nrow(d),size=nrow(d)/2)
d1 <- d[ i , ]
d2 <- d[ -i , ]

f1 <- alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b1*age,
        c(a, b1) ~ dnorm(0, 100) ,
        sigma ~ dunif(0 , 50)
        )
f2 <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age**2,
  c(a, b1, b2) ~ dnorm(0, 100) ,
  sigma ~ dunif(0 , 50)
)
f3 <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age**2 + b3*age**3,
  c(a, b1, b2, b3) ~ dnorm(0, 100) ,
  sigma ~ dunif(0 , 50)
)
f4 <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age**2 + b3*age**3 + b4*age**4,
  c(a, b1, b2, b3, b4) ~ dnorm(0, 100) ,
  sigma ~ dunif(0 , 50)
)
f5 <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age**2 + b3*age**3 + b4*age**4 + b5*age**5,
  c(a, b1, b2, b3, b4, b5) ~ dnorm(0, 100) ,
  sigma ~ dunif(0 , 50)
)
f6 <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age**2 + b3*age**3 + b4*age**4 + b5*age**5 + b6*age**6,
  c(a, b1, b2, b3, b4, b5, b6) ~ dnorm(0, 100),
  sigma ~ dunif(0 , 50)
)

a.start <- mean(d1$height)
sigma.start <- sd(d1$height)

# fit models
m1 <- map( f1 , data=d1 ,
           start=list(a=a.start,sigma=sigma.start,b1=0) )
m2 <- map( f2 , data=d1 ,
           start=list(a=a.start,sigma=sigma.start,b1=0,b2=0) )
m3 <- map( f3 , data=d1 ,
           start=list(a=a.start,sigma=sigma.start,b1=0,b2=0,
                      b3=0) )
m4 <- map( f4 , data=d1 ,
           start=list(a=a.start,sigma=sigma.start,b1=0,b2=0,
                      b3=0,b4=0) )
m5 <- map( f5 , data=d1 ,
           start=list(a=a.start,sigma=sigma.start,b1=0,b2=0,
                      b3=0,b4=0,b5=0) )
m6 <- map( f6 , data=d1 ,
           start=list(a=a.start,sigma=sigma.start,b1=0,b2=0,
                      b3=0,b4=0,b5=0,b6=0) )

compare(m1, m2, m3, m4, m5, m6)

ci_pi_plot <- function(model, prob){
plot(height ~ age, d , col=rangi2, xlim = c(-2, 3))
age.seq <- seq(-3, 3, by = 0.01)
preds <- link(model, data=data.frame(age = age.seq))
mu <- apply(preds, 2, mean)
# prediction interval for mu
mu.PI <- apply(preds, 2, PI, prob = prob)
# confidence interval for target height - simulate posterior observations
h <- sim(model, data = data.frame(age = age.seq))
h.CI <- apply(h, 2, PI, prob = prob)
# plot it all
lines(age.seq, mu, lty=2)
shade(mu.PI, age.seq)
shade(h.CI, age.seq)
}
model_vec <- c(m1, m2, m3, m4, m5, m6)
par(mfrow = c(2, 3))
sapply(model_vec, ci_pi_plot, prob = 0.97)

#6H3
height.ensemble <- ensemble(m1, m2, m3, m4, m5, m6, data=data.frame(age = age.seq))
mu <- apply(height.ensemble$link, 2, mean)
mu.ci <- apply(height.ensemble$link , 2 , PI )
height.ci <- apply(height.ensemble$sim, 2, PI)
par(mfrow = c(1,2))
plot(height ~ age, d, col =rangi2, xlim = c(-2, 3))
lines(age.seq , mu )
shade(mu.ci , age.seq)
shade(height.ci , age.seq)

ci_pi_plot(m4, prob = 0.97)

#6H4 & 6H5
# calculate out of sample deviance
test_dev <- function(model, new_data){
  preds <- link(model, new_data)
  mu <- apply(preds, 2, mean)
  -2*sum(dnorm(new_data$height, mu, coef(model)['sigma'], log=TRUE))
}
deviance_results <- sapply(model_vec, test_dev, new_data = d2)
names(deviance_results) <- paste0('m', seq(1, 6))
as.data.frame(deviance_results[order(deviance_results)])
compare(m1, m2, m3, m4, m5, m6)

#6H6
# regularized priors on b's, flat priors on a & sigma
f6.6 <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age**2 + b3*age**3 + b4*age**4 + b5*age**5 + b6*age**6,
  c(b1, b2, b3, b4, b5, b6) ~ dnorm(0, 5)
)
m6.6 <- map(f6.6, data = d1,
            start = list(
              a = mean(d$height),
              b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0,
              sigma = sd(d$height)
            ))
coef(m6.6)
ci_pi_plot(m6.6, prob = 0.97)
test_dev(m6.6, d2)
compare(m6.6)
compare(m6.6, m4)
