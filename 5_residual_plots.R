library(rethinking)

data(WaffleDivorce)
d <- WaffleDivorce

colnames(d)

pred_standardizer <- function(data, var){
(data[[var]] - mean(data[[var]]))/sd(data[[var]])
  #return(data)
}

var_list <- c('Population', 'MedianAgeMarriage', 'Marriage', 'Marriage.SE', 'Divorce', 'Divorce.SE',
              'WaffleHouses', 'Slaves1860', 'Population1860', 'PropSlaves1860')
  
for(var in var_list){
  d[[paste0(var, '.s')]] <- pred_standardizer(d, var)
}


residual_plot <- function(resp, pred, data){
# fit model
data$response <- data[[resp]]
data$predictor <- data[[pred]]

resid_model <- map(
  alist(
    response ~ dnorm(a + bA * predictor, sigma) ,
    #mu <-  ,
    a ~ dnorm( 0 , 100 ) ,
    bA ~ dnorm( 0 , 100 ) ,
    sigma ~ dunif( 0 , 100 )
  ) , data = data )

resid_model
# compute expected value at MAP, for each State
mu <- coef(resid_model)['a'] + coef(resid_model)['bA']*data$predictor
# compute residual for each State
m.resid <- data$response - mu
plot(x = m.resid, y = data$Divorce, xlab = 'Residual',
     ylab = 'Target', main = 'Residual Predictor Plot')
}

par(mfrow=c(1,2))
plot(Divorce ~ Population, d[d$Population < 15,], xlab = 'Predictor', ylab = 'Target')
residual_plot(resp = 'Population', pred = 'MedianAgeMarriage.s',
              data = d[d$Population < 15,])

# marriage is predictive of divorce, controlling for population
par(mfrow=c(1,2))
plot(Divorce ~ Marriage, d, xlab = 'Predictor', ylab = 'Target', main = 'Target vs. Predictor')
residual_plot(resp = 'Marriage', pred = 'Population.s', d)

residual_plot(resp = 'avgfood', pred = 'groupsize.s')
residual_plot(resp = 'area', pred = 'avgfood.s')
residual_plot(resp = 'area', pred = 'group.s')
residual_plot(resp = 'area', pred = 'groupsize')
residual_plot(resp = 'groupsize', pred = 'area')


plot(get(response) ~ get(pred) , d , col=rangi2 )
abline( resid_model )
# loop over States
for ( i in 1:length(m.resid) ) {
  x <- d$group.s[i] # x location of line segment
  y <- d$avgfood[i] # observed endpoint of line segment
  # draw the line segment
  lines( c(x,x) , c(mu[i],y) , lwd=0.5 , col=col.alpha("black",0.7) )
}

