library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce
#5M4
d$pct_LDS <- c(0.75, 4.53, 6.18, 1, 2.01, 2.82, 0.43, 0.55, 0.38,
               0.75, 0.82, 5.18, 26.35, 0.44, 0.66, 0.87, 1.25, 0.77, 0.64, 0.81,
               0.72, 0.39, 0.44, 0.58, 0.72, 1.14, 4.78, 1.29, 0.61, 0.37, 3.34,
               0.41, 0.82, 1.48, 0.52, 1.2, 3.85, 0.4, 0.37, 0.83, 1.27, 0.75,
               1.21, 67.97, 0.74, 1.13, 3.99, 0.92, 0.44, 11.5 )
lds_map <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bL*pct_LDS + bM*Marriage + bd*MedianAgeMarriage,
    a ~ dnorm(0, 100),
    bL ~ dnorm(0, 10),
    bM ~ dnorm(0, 10),
    bd ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)),
  data=d)

m_5M4 <- map(
  alist(
    Divorce ~ dnorm(mu,sigma),
    mu <- a + bR*Marriage + bA*MedianAgeMarriage + bM*pct_LDS,
    a ~ dnorm(0,100),
    c(bA,bR,bM) ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=d )

precis(lds_map)
precis(m_5M4)

data("foxes")
d <- foxes
#5H1
terr_wgt <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bA*area,
    a ~ dnorm(0, 100),
    bA ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data=d)

grp_wgt <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bG*groupsize,
    a ~ dnorm(0, 100),
    bG ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data=d)

# area looks terrible
area.seq <- seq(1, 6, length.out = 1000)
t <- link(terr_wgt, n = 1e4, data = data.frame(area = area.seq))
t.mu <- apply(t, 2, mean)
t.HPDI <- apply(t, 2, HPDI)
plot(weight ~ area, data=d)
abline(terr_wgt)
shade(t.HPDI, area.seq)
# group size might show some negative correlation
group.seq <- seq(2, 8, by = 1)
t <- link(grp_wgt, n = 1e4, data = data.frame(groupsize = group.seq))
t.mu <- apply(t, 2, mean)
t.HPDI <- apply(t, 2, HPDI)
plot(weight ~ groupsize, data=d)
abline(grp_wgt)
shade(t.HPDI, group.seq)

#5H2
H2 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bG*groupsize + bA*area,
    a ~ dnorm(0, 100),
    bG ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data=d)
precis(H2)

# area now has positive coefficient, holding group size constant
grp.mu <- mean(d$groupsize)
area.seq <- seq(1, 6, length.out = 1000)
t <- link(H2, n = 1e4, data = data.frame(area = area.seq,
                                               groupsize = grp.mu))
t.mu <- apply(t, 2, mean)
t.HPDI <- apply(t, 2, HPDI)
plot(weight ~ area, data=d)
lines(x = area.seq, y = t.mu)
shade(t.HPDI, area.seq)
# group size now has higher magnitude, negative coefficient
area.mu <- mean(d$area)
group.seq <- seq(2, 8, by = 1)
t <- link(H2, n = 1e4, data = data.frame(area = area.mu,
                                         groupsize = group.seq))
t.mu <- apply(t, 2, mean)
t.HPDI <- apply(t, 2, HPDI)
plot(weight ~ groupsize, data=d)
lines(x = group.seq, y = t.mu)
shade(t.HPDI, group.seq)
# result since group size positive with area
# group size negative with weight (less food)
# area positive with weight (more resources)

#5H3
H3.1 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bA*avgfood + bG*groupsize,
    a ~ dnorm(0, 100),
    bA ~ dnorm(0, 10),
    bG ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data=d)

H3.2 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bA*avgfood + bG*groupsize + bAe*area,
    a ~ dnorm(0, 100),
    bA ~ dnorm(0, 10),
    bG ~ dnorm(0, 10),
    bAe ~ dnorm(0,10),
    sigma ~ dunif(0, 10)
  ),
  data=d)
precis(H3.1)
precis(H3.2)
H3.1_w <- link(H3.1, n = 1e4)
H3.2_w <- link(H3.2, n = 1e4)
# a) avgfood or groupsize better predictors?
food.seq <- seq(0, 1.5, length.out = 100)
food.pred <- data.frame(avgfood = food.seq, groupsize = mean(d$groupsize))
group.seq <- seq(0, 8, length.out = 100)
group.pred <- data.frame(avgfood = mean(d$avgfood), groupsize = group.seq)
food.w <- link(H3.1, data = food.pred, n = 1e4)
group.w <- link(H3.1, data = group.pred, n = 1e4)
food.mu <- apply(food.w, 2, mean)
group.mu <- apply(group.w, 2, mean) 
food.HPDI <- apply(food.w, 2, HPDI)
group.HPDI <- apply(group.w, 2, HPDI)
# avg food
par(mfrow=c(1,2))
plot(weight ~ avgfood, data = d, type = 'n')
lines(food.seq, food.mu)
lines(food.seq, food.HPDI[1,], lty=2)
lines(food.seq, food.HPDI[2,], lty=2)
# group size
plot(weight ~ groupsize, data = d, type = 'n')
lines(group.seq, group.mu)
lines(group.seq, group.HPDI[1,], lty=2)
lines(group.seq, group.HPDI[2,], lty=2)
# b) explain model H3.2
precis(H3.1)
precis(H3.2)
pairs(d[,c(1:5)], labels = paste0('var', seq(1:5)), xaxt = 'n', yaxt = 'n')
H3.1_mu <- apply(H3.1_w, 2, mean)
H3.2_mu <- apply(H3.2_w, 2, mean)
H3.1_HPDI <- apply(H3.1_w, 2, HPDI)
H3.2_HPDI <- apply(H3.2_w, 2, HPDI)

d$H3.1_resid <- d$weight - H3.1_mu
d$H3.2_resid <- d$weight - H3.2_mu
par(mfrow = c(2,1))
plot(H3.1_resid ~ weight, data = d)
plot(H3.2_mu, d$weight)
plot(H3.2_resid ~ weight, data = d)
