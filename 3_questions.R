library(rethinking)
# setup prior + posterior
p_grid <- seq( from=0 , to=1 , length.out=10000)
prior <- rep(1, 10000)
likelihood <- dbinom(6, size= 9, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE)

plot(x = p_grid, y = posterior, xlab = 'Renewal Rate', ylab = 'Density',
     yaxt = 'n')
axis(2, at = c(0, .0001, .0002), labels=c('0', '.0001', '.0002'))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 

#3e1
plot(x = p_grid, y = posterior)
x.1 <- sum(posterior[1:(.2*length(posterior))])
# .0008635326

#3e2
x.2 <- sum(posterior[(.8*length(posterior)):length(posterior)])
# .1209972

#3e3
1-(x.1 + x.2)
# .8781392

#3e4
posterior_cdf <- cumsum(posterior)
p_grid[which(posterior_cdf > .2)[1]]
#.5163516

#3e5
p_grid[which(posterior_cdf > .8)[1]]
#.7605761

#3e6
HPDI(samples, 0.66)
# [0.5181518, 0.7923792]

#3e7
quantile(samples, (1-.66)/2)
quantile(samples, 1-(1-.66)/2)
# [0.49743, 0.7757946]

#3M1
library(rethinking)
# setup prior + posterior
p_grid <- seq( from=0 , to=1 , length.out=10000)
prior <- rep(1, 10000)
likelihood <- dbinom(8, size= 15, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot(y = posterior, x = p_grid)

#3M2
samples <- sample(p_grid, size = 1e4, prob = posterior, replace = T)
HPDI(samples, .9)

#3M3
w <- rbinom(n = 1e4, 15, samples)
sum(w == 8)/length(w)
# 0.1461

#3M4
w <- rbinom(n = 1e4, 9, samples)
sum(w == 6)/length(w)
# 0.175

#3M5
library(rethinking)
# setup prior + posterior
p_grid <- seq( from=0 , to=1 , length.out=10000)
prior <- c(rep(0, 5000), rep(1, 5000))
likelihood <- dbinom(8, size= 15, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot(y = posterior, x = p_grid)

samples <- sample(p_grid, size = 1e4, prob = posterior, replace = T)
HPDI(samples, .9)
# [0.5, 0.7141714]
w <- rbinom(n = 1e4, 15, samples)
sum(w == 8)/length(w)
#.1561
w <- rbinom(n = 1e4, 9, samples)
sum(w == 6)/length(w)
#0.2283

#Hard question setup
birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
            0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
            1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
            1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
            1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
            0,0,0,1,1,1,0,0,0,0)

## R code 3.29
library(rethinking)
data(homeworkch3)

## R code 3.30
sum(birth1) + sum(birth2)

#3H1
p_grid <- seq( from=0 , to=1 , length.out=10000)
prior <- rep(1, 10000)
likelihood <- dbinom(sum(birth1) + sum(birth2), size= 200, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot(y = posterior, x = p_grid)
samples <- sample(p_grid, size = 1e4, prob = posterior, replace = T)
chainmode(samples)
# 0.55365457

#3H2
HPDI(samples, prob = c(.5, .89, .97))
# [.533, .578] [.49975, .61186] [.4787, .63056]

#3H3
w <- rbinom(1e4, 200, samples)
dens(w, adj = 0.1)
abline(v = 111, col = 'red')

#3H4
w <- rbinom(1e4, 100, samples)
dens(w, adj = 0.1)
abline(v = sum(birth1), col = 'red')

#3H5
b1_female <- sum(birth1 == 0)
w <- rbinom(1e4, b1_female, samples)
dens(w, adj = 0.1)
b2_given_b1_female <-birth2[which(birth1 == 0)]
abline(v = sum(b2_given_b1_female), col = 'red')

b1_male <- sum(birth1 == 1)
w <- rbinom(1e4, b1_male, samples)
dens(w, adj = 0.1)
b2_given_b1_male <-birth2[which(birth1 == 1)]
abline(v = sum(b2_given_b1_male), col = 'red')

