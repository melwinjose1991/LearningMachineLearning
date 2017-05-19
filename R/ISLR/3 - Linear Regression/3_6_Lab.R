library(MASS)
library(ISLR)

names(Boston)
attach(Boston)


## Simple Linear Regression
lm.fit = lm(medv~lstat)

plot(lstat, medv) # see the non-linearity in the relationship
abline (lm.fit ,lwd =3, col ="red ")

par(mfrow=c(2,2))
plot(lm.fit)

plot(hatvalues(lm.fit))



## Multiple Linear Regression
