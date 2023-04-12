# Q4
# a
load("dataex4.Rdata")
require(mice)
impa <- mice(dataex4, maxit = 20,m = 50, seed = 1, printFlag = F)
fita <- with(impa, lm(y~x1+x2+x1:x2))
esta <- pool(fita)
summary(esta, conf.int = T)
# b
data<- dataex4
data['x1x2']=dataex4$x1*dataex4$x2
imp0 <- mice(data, maxit=0)
imp0
meth <- imp0$method
meth["x1x2"] <- "~I(x1*x2)"
pred <- imp0$predictorMatrix
# x1x2 will not be used as predictor of x1 and x2
pred[c("x1","x2"),"x1x2"] <- 0
# x1x2 will not act as a predictor of in the x1 and x2 imputation models.
pred[,c("x1","x2")] <- 0
# x1, x2 should be the predictor of each other
pred["x1","x2"] <- 1
pred["x2","x1"] <- 1
pred
impb <- mice(data, method = meth, predictorMatrix = pred, maxit = 20, m = 50, seed = 1, printFlag = F)
impb
fitb <- with(impb, lm(y~x1+x2+x1:x2))
estb <- pool(fitb)
summary(estb, conf.int = T)
# c
impc <- mice(data, maxit = 20, m = 50, seed = 1, printFlag = F)
fitc <- with(impc, lm(y~x1+x2+x1:x2))
estc <- pool(fitc)
summary(estc, conf.int = T)