library(glmnet)
library(ISLR)

attach(Hitters)

x = model.matrix(Salary~., Hitters)[,-1]
y = Hitters[row.names(x),]$Salary



## Ridge Regression
grid = 10 ^ seq(10,-2, length=100)
ridge.model = glmnet(x, y, alpha=0, lambda=grid)

ridge.model$lambda[60]
coef(ridge.model)[,60]
sqrt(sum(coef(ridge.model)[-1,60]^2))

predict(ridge.model, s=50, type="coefficients")[1:20,]  

set.seed(1)
train = sample(1:nrow(x), nrow(x)*0.75)
test = (-train)
y.test = y[test]

ridge_cv_model =cv.glmnet(x[train, ], y[train], alpha=0, lambda=grid)
plot(ridge_cv_model)
best_lambda = ridge_cv_model$lambda.min
best_lambda

pred = predict(ridge.model, s=best_lambda, newx=x[test,])
mean((pred-y.test)^2)



## Lasso
lasso.model = glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.model)

lasso_cv_model =cv.glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso_cv_model)
best_lambda = lasso_cv_model$lambda.min
best_lambda

pred = predict(lasso.model, s=best_lambda, newx=x[test,])
mean((pred-y.test)^2)

lasso.coef = predict(lasso.model, type="coefficient", s=best_lambda)[1:20,]
lasso.coef[lasso.coef!=0]   # only non-zero coefficients

