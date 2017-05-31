library(ISLR)
library(corrplot)

attach(Smarket)
head(Smarket)
s


# 4.6.1
corrplot.mixed(cor(Smarket[,-9]), upper="circle", lower="number")
# the average number of shares traded daily increased from 2001 to 2005.



# 4.6.2
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,data=Smarket ,family =binomial)
summary(glm.fit)



# 4.6.3
train =(Year <2005)
Smarket.2005= Smarket [!train ,]
Direction.2005= Direction [!train]

library (MASS)
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005)

head(lda.pred$class)
head(lda.pred$posterior)
head(lda.pred$x)



# 4.6.5

