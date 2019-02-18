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
     
