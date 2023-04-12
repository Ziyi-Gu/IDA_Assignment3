# Q1
# (a)
p <- 1- nrow(cc(nhanes))/nrow(nhanes)
p
# (b)
require(mice)
imp1 <- mice(nhanes,printFlag = F, m = 5,set.seed(1))
imp1
fit1 <- with(imp1, lm(bmi ~ age + hyp + chl))
est1 <- pool(fit1)
est1
# (c)
est2 <- pool(with(mice(nhanes,printFlag = F, m = 5,set.seed(2)),lm(bmi ~ age + hyp + chl)))
est3 <- pool(with(mice(nhanes,printFlag = F, m = 5,set.seed(3)),lm(bmi ~ age + hyp + chl)))
est4 <- pool(with(mice(nhanes,printFlag = F, m = 5,set.seed(4)),lm(bmi ~ age + hyp + chl)))
est5 <- pool(with(mice(nhanes,printFlag = F, m = 5,set.seed(5)),lm(bmi ~ age + hyp + chl)))
est6 <- pool(with(mice(nhanes,printFlag = F, m = 5,set.seed(6)),lm(bmi ~ age + hyp + chl)))
est2;est3;est4;est5;est6
# (d)
est1 <- pool(with(mice(nhanes,printFlag = F, m = 100,set.seed(1)),lm(bmi ~ age + hyp + chl)))
est2 <- pool(with(mice(nhanes,printFlag = F, m = 100,set.seed(2)),lm(bmi ~ age + hyp + chl)))
est3 <- pool(with(mice(nhanes,printFlag = F, m = 100,set.seed(3)),lm(bmi ~ age + hyp + chl)))
est4 <- pool(with(mice(nhanes,printFlag = F, m = 100,set.seed(4)),lm(bmi ~ age + hyp + chl)))
est5 <- pool(with(mice(nhanes,printFlag = F, m = 100,set.seed(5)),lm(bmi ~ age + hyp + chl)))
est6 <- pool(with(mice(nhanes,printFlag = F, m = 100,set.seed(6)),lm(bmi ~ age + hyp + chl)))
est1;est2;est3;est4;est5;est6