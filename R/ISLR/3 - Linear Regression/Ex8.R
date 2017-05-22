library(ISLR)
library(car)
library(MASS)

data(Auto)



# 8
  # a
  fit = lm(mpg ~horsepower, data=Auto)
  summary(fit)

  predict(fit, data.frame(horsepower=c(98)), interval="confidence")

  predict(fit, data.frame(horsepower=c(98)), interval="prediction")

  # b
  plot(Auto$horsepower, Auto$mpg)
  abline(fit)
  
  # c
  par(mfrow=c(2,2))
  plot(fit)
  # non-linear relationship
  

  
# 9
  #a
  pairs(Auto)
  
  #b
  cor(subset(Auto, select=-name))
  
  #c
  fit_all = lm(mpg~.-name, data=Auto)
  summary(fit_all)
  
  #d 
  par(mfrow=c(2,2))
  plot(fit_all)
   # std.residual vs leverage : point_14
  
  plot(predict(fit_all), rstudent(fit_all))
   # there are points outside the -3to+3 interval
  
  #e
  fit_interactions = lm(mpg~cylinders*displacement+displacement*weight, data=Auto)
  summary(fit_interactions)
  
  #f 
  fit_all = lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
  summary(fit_all)
  # R = 0.821
  plot(fit_all)
  
  
  # Transforming predictors
  #   https://stats.stackexchange.com/questions/61217/transforming-variables-for-multiple-regression-in-r
  
  cor(Auto$mpg, Auto$displacement)         # 0.805
  boxTidwell(mpg~displacement, data=Auto) 
  cor(Auto$mpg, Auto$displacement^-0.12)   # 0.829
  fit = lm(mpg~cylinders+I(displacement^-0.12)+horsepower+weight+acceleration+year+origin,data=Auto)
  summary(fit)
  # 0.826
  plot(fit)
  
  cor(Auto$mpg, Auto$horsepower)          # 0.778
  boxTidwell(mpg~horsepower, data=Auto)
  cor(Auto$mpg, Auto$horsepower^-0.485)   # 0.822
  fit = lm(mpg~cylinders+I(displacement^-0.12)+I(horsepower^-0.485)+weight+acceleration+year+origin,data=Auto)
  summary(fit)
  # 0.848
  plot(fit)
  
  cor(Auto$mpg, Auto$weight)              # 0.832
  boxTidwell(mpg~weight, data=Auto)
  cor(Auto$mpg, Auto$weight^-0.274)       # 0.844
  fit = lm(mpg~cylinders+I(displacement^-0.12)+I(horsepower^-0.485)+I(weight^-0.274)+
             acceleration+year+origin,data=Auto)
  summary(fit)
  # 0.8556
  plot(fit)
  
  
  # Tranforming the dependent variable
  boxCox(fit, plotit=TRUE)
  
  bc = boxCox(fit_all, plotit=TRUE)
  lambda = bc$x[which.max(bc$y)]
  fit_all = lm(I(mpg^lambda)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
  summary(fit_all)
  # 0.888
  
  
  
#10
  #a-b
  names(Carseats)
  fit_1 = lm(Sales~Price+Urban+US, data=Carseats)
  summary(fit_1)
  
  #e 
  fit_2 = lm(Sales~Price+US, data=Carseats)
  summary(fit_2)
  
  #f
  #  Based on the RSE and R^2 of the linear regressions, they both fit 
  #  the data similarly, with linear regression from (e) fitting the data slightly better
  
  #g
  confint(fit_2)
  
  #h
  plot(fit_2)
  # (p+1)/n = 0.0075, lots of points beyond that, they are 
  # high leverage points
  
  plot(predict(fit_2), rstudent(fit_2))
  # everything is within the -3,+3 interval, NO outliers
  

  
#11
  
  #
  set.seed(1)
  x = rnorm(100)
  y = 2*x + rnorm(100)

  lm.fit = lm(y~x+0)
  summary(lm.fit)  
  
  
  
# 13
  set.seed(1)
  x = rnorm(100)
  eps = rnorm(100, 0, sqrt(0.25))
  y = -1 + 0.5*x + eps
  plot(x, y)
  
  lm.fit = lm(y~x)
  summary(lm.fit)
  
  #f
  par(mfrow=c(1,1))
  plot(x, y)
  abline(lm.fit, lwd=3, col=2)
  abline(-1, 0.5, lwd=3, col=3)
  legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)  
  
  #g
  lm.fit = lm(y~x+I(x^2))
  summary(lm.fit)
  # R^2 and RSE indicates it is a better model
  # but p-value of I(X^2) says it is insignificant

    

#15
  names(Boston)
  fit = lm(crim~., data=Boston)  
  summary(fit)  
  