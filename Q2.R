# Q2
load('dataex2.Rdata')
# stochastic regression imputation
n1 <- 0
for (i in 1:100){
  impi <- mice(dataex2[,,i], method = "norm.nob", printFlag = F, m = 20, seed = 1)
  esti <- pool(with(impi,lm(Y~X)))
  lb <- summary(esti,conf.int = T)[2,7]
  ub <- summary(esti,conf.int = T)[2,8]
  if (lb<=3 & ub>=3){
    n1 <- n1+1
  }
}
prob1 <- n1/100
prob1
# bootstrap 
n2 <- 0
for (i in 1:100){
  impi <- mice(dataex2[,,i], method = "norm.boot", printFlag = F, m = 20, seed = 1)
  esti <- pool(with(impi,lm(Y~X)))
  lb <- summary(esti,conf.int = T)[2,7]
  ub <- summary(esti,conf.int = T)[2,8]
  if (lb<=3 & ub>=3){
    n2 <- n2+1
  }
}
prob2 <- n2/100
prob2