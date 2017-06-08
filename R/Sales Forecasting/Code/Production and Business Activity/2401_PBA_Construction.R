library(corrplot)
library(leaps)
library(glmnet)

source("../Common/Utils.R")



# Loading Data
data_revenue = read.csv("../../Data/2401_Revenue.csv", header=TRUE, sep=",")
data_vars = read.csv("../../Data/Production and Business Activity/Construction_nsa.csv", header=TRUE, sep=",")

data = cbind(data_revenue[,c("orders_rcvd","month")], data_vars)
data$month = as.factor(data$month)
data = data[,!names(data) %in% c("date")]



## Correlation
corrplot.mixed(cor(data[,!names(data) %in% c("month")]), upper="circle", lower="number")
# Highest Correlation : 0.410 : PBPWRCON : Total Public Construction Spending: Power

# Removing highly correlated variables
data_num_var = data[,!names(data) %in% c("month","orders_rcvd")]
tmp = cor(data_num_var)
tmp[!lower.tri(tmp)] = 0
uncorrelated_vars = names(data_num_var[,!apply(tmp,2,function(x) any(x > 0.95))])
uncorrelated_vars
corrplot.mixed(cor(data[,uncorrelated_vars]), upper="circle", lower="number")

data.new = data[,c("orders_rcvd","month",uncorrelated_vars)]
corrplot.mixed(cor(data.new[,!names(data.new) %in% c("month")]), upper="circle", lower="number")



## Parameters
no_vars = 5 #dim(data.new)[2]/2
method = "exhaustive" # exhaustive, forward
var_cols = names(data.new)



## Cross-Validated Model Selection
source("../Common/Utils.R")

mean_errors = cvSubsetSelection(data.new, no_vars=no_vars, method=method)
mean_errors
best_model = which.min(mean_errors)
best_model

leaps = regsubsets(orders_rcvd~., data=data[,var_cols], nvmax=no_vars, method=method)
coef(leaps,id=best_model)

# best model with the least CV'd MAE - limited to nvmax=5
# Not Seasonally Adjusted
#   forward    : PBPWRCON : 3581
#   exhaustive : PBPWRCON : 3581
# Seasonally Adjusted Annual Rate
#   forward    : TLWSCONS : 3441
#   exhaustive : TLWSCONS : 3441



## Leaps' Model Selection
#leaps = regsubsets(orders_rcvd~., data=data[,var_cols], nvmax=length(var_cols), method="forward", 
#                   force.in=1:11)
#leaps = regsubsets(orders_rcvd~., data=data[,var_cols], nvmax=length(var_cols)+11, method="forward")
leaps = regsubsets(orders_rcvd~., data=data[,var_cols], nvmax=no_vars, method=method)
plot(leaps)

leaps_summary = summary(leaps)

best_model = which.min(leaps_summary$bic)
coef(leaps,id=best_model)

best_model = which.max(leaps_summary$adjr2)
coef(leaps,id=best_model)

# best model with regsubsets Adj-R2 - limited to nvmax=5
# forward    : month4 + month10 + month11 + PBPWRCON + TLWSCON 
# exhaustive : PBAMUSCON + TLCADCON + TLEDUCON + TLOFCON + TLSWDCON



## LASSO Regression
grid = 2.71828^seq(0.001, 10, length=10000)

x = model.matrix(orders_rcvd~., data)[,-1]
y = data$orders_rcvd

cv.l2.fit = cv.glmnet(x, y, alpha=1, type.measure="mae", lambda=grid)
plot(cv.l2.fit)

best_lambda = cv.l2.fit$lambda.min
best_lambda
best_lambda_index = match(best_lambda, cv.l2.fit$lambda)
best_lambda_index

cv.l2.fit$cvm[best_lambda_index]

l2.fit = glmnet(x, y, alpha=1, lambda=best_lambda)
coef(l2.fit)

# run#   MAE       month8 + month10 + month11 + PBPWRCON
#    1   3557         *                  *         *
#    2   3568         *                  *         *
#    3   3505         *                  *         *
#    4   3554                                      *
