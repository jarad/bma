library(BMS)
library(BMA)
library(plyr)

set.seed(20150909)

n_reps = 5
indicator = data.frame(I = rep(c(0,1), each = 2*n_reps),
               D = rep(c(0,1), each = n_reps, times= 2))
indicator$ID = indicator$I*indicator$D

b = rep(1,3)
indicator$y = rnorm(nrow(d), 1+as.matrix(d[,c("I","D","ID")])%*%b)

sum_to_zero = data.frame(I = ifelse(indicator$I, -1, 1),
               D = ifelse(indicator$D, -1, 1))
sum_to_zero$ID = sum_to_zero$I*sum_to_zero$D
sum_to_zero$y = indicator$y


# R squared
models = list("y~1",
  "y~1    +I:D",
  "y~1  +D",
  "y~1  +D+I:D",
  "y~1+I",
  "y~1+I  +I:D",
  "y~1+I+D",
  "y~1+I+D+I:D")

r2 = 
ldply(models, function(x) {
  mi = lm(x, indicator)
  ms = lm(x, sum_to_zero)
  data.frame(indicator = summary(mi)$r.squared,
             sum_to_zero = summary(ms)$r.squared)
})

cbind(unlist(models), r2)


bma = bicreg(d[,c("I","D","ID")], indicator$y, OR=Inf)
imageplot.bma(bma)


# 
standard = bms(d[,c("y","I","D","ID")], mcmc='enumerate', g='EBL')
tmp = as.data.frame(print(standarindicator$topmod))

standarindicator$post_mod_prob = ml/sum(ml)

o = data.frame(I = ifelse(indicator$I, -1, 1),
               D = ifelse(indicator$D, -1, 1))
sum_to_zero$ID = sum_to_zero$I*sum_to_zero$D

orthogonal = bms(cbind(indicator$y, o), mcmc='enumerate')
