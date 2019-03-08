library(rethinking)

sppnames <- c( "afarensis","africanus","habilis","boisei",
               "rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

# simple linear model
m6.1 <- lm( brain ~ mass , data=d )
# fit polynomial model
m6.2 <- lm( brain ~ mass + I(mass^2) , data=d )

# plot confidence interval
plot(brain ~ mass, data = d, type = 'n',
     main = paste0('R^2 = ', round((1 - var(resid(m6.1))/var(d$brain)), 4)))
mass.seq <- seq(30, 65, 1)
brain_preds <- predict(m6.1, newdata = data.frame(mass = mass.seq),interval = "confidence")
polygon(c(rev(mass.seq), mass.seq), c(rev(brain_preds[,2]), brain_preds[,3]),
        col = 'grey80', border = NA)
abline(m6.1)
lines(mass.seq, brain_preds[,2], lty = 'dashed', col = 'red')
lines(mass.seq, brain_preds[,3], lty = 'dashed', col = 'red')
points(brain ~ mass, data = d)  

# derive confidence interval, same method as lm
# pull out QR matrix
XRinv <- qr.Q(m6.1$qr)
# derive prediction mean squared error
res.var <- sum(m6.1$residuals**2)/m6.1$df.residual
# derive population variance estimate
ip <- XRinv**2 %*% rep(res.var, 2)
# t-statistics
tfrac <- qt((1 - .95)/2, m6.1$df.residual)
# confidence interval width
hwid <- sqrt(ip)*tfrac
brain_preds <- predict(m6.1,interval = "confidence")
brain_preds[,1]+hwid
brain_preds[,1]-hwid
# prediction interval - additional MSE term!
pwid <- sqrt(ip + res.var)*tfrac
brain_preds <- predict(m6.1, interval = "prediction")

# calculate confidence interval w/o QR decompostion
X <- cbind(1, d$mass)
Xt <- t(X)
Xh <- cbind(1, d$mass)
# only need diagonal on the outer product... better way to do this?
mywid <- drop(tfrac*sqrt(res.var*diag(Xh%*%MASS::ginv(Xt%*%X)%*%t(Xh))))
brain_preds[,1] + mywid
brain_preds[,1] - mywid
brain_preds
