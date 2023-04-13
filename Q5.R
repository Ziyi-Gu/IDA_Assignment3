# Q5
load("NHANES2.Rdata")
# explore some basic information of the dataset
dim(NHANES2)
str(NHANES2)
summary(NHANES2)
require(mice)
md <- md.pattern(NHANES2)
md
require(JointAI)
md_pattern(NHANES2, pattern = FALSE, color = c('#34111b','#e30f41'))
# There are 411 observations with observed values on all 12 variables

# step 1
imp <-  mice(NHANES2, maxit = 20, m = 30, seed = 1, printFlag = F)
imp$loggedEvents
plot(imp, layout = c(6,6))
densityplot(imp)
densityplot(imp, ~ SBP|hypten + gender)
densityplot(imp, ~hgt|gender)
require(devtools)
require(reshape2)
require(RColorBrewer)
require(ggplot2)
source_url("https://gist.githubusercontent.com/NErler/0d00375da460dd33839b98faeee2fdab/raw/c6f537ecf80eddcefd94992ec7926aa57d454536/propplot.R")
propplot(imp)

# step 2
fit <- with(imp,lm(wgt ~ gender + age + hgt + WC))
summary(fit$analyses[[1]])
plot(fit$analyses[[1]]$fitted.values,residuals((fit$analyses[[1]])), xlab = "Fitted values", ylab = "Residuals")
# check normality
qqnorm(rstandard(fit$analyses[[1]]), xlim = c(-4,4), ylim = c(-4,4))
qqline(rstandard(fit$analyses[[1]]), col = 2)

# step 3
ests <- pool(fit)
ests
summary(ests, conf.int = T)
fit_no_gender <- with(imp,lm(wgt ~ age + hgt + WC))
D1(fit, fit_no_gender)
summary(pool(fit_no_gender), conf.int = T)
imp50 <- mice(NHANES2, maxit = 20, m = 50, seed = 1, printFlag = F)
D1(with(imp50,lm(wgt ~ gender + age + hgt + WC)),with(imp50,lm(wgt ~  age + hgt + WC)))
