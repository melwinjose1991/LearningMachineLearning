library(corrplot)
library(leaps)
library(glmnet)

source("../Common/Utils.R")



## Parameters
data_folder = "../../Data/"
sa_OR_nsa = "Not Seasonally Adjusted"

# Loading Data
revenue_file = paste0(data_folder,"2401/Revenue.csv")
data_revenue = read.csv(revenue_file)
data = data_revenue[,c("orders_rcvd","month","t")]
data$month = as.factor(data$month)

## FRED
FRED_folder = paste0(data_folder, "External Data/FRED/")
meta_data_file = paste0(FRED_folder, "meta_data.csv")
config_data = read.csv(meta_data_file)


for(sub_category_id in unique(config_data$sub_category_id)){
  
  category_name = unique(config_data[config_data$sub_category_id==sub_category_id,"category_name"])
  sub_category_name = unique(config_data[config_data$sub_category_id==sub_category_id,"sub_category_name"])
  
  
  file = paste0(FRED_folder ,as.character(category_name), "/", as.character(sub_category_name))
  if(sa_OR_nsa=="Not Seasonally Adjusted"){
    file = paste0(file, "_nsa.csv")
  }else{
    file = paste0(file, "_sa.csv")
  }
  print(paste0("Reading file : ",file))
  data_vars = read.csv(file, header=TRUE, sep=",")

  data_vars = data_vars[,!names(data_vars) %in% c("date")]
  data = cbind(data, data_vars)
  
}



### Filtering Techinques

# Removing columns whose values don't change
data = data[sapply(data, function(x) length(unique(x))>1)]

## 1 : Correlation
# corrplot.mixed(cor(data[,!names(data) %in% c("month")]), upper="circle", lower="number")
# Highest Correlation : 0.410 : PBPWRCON : Total Public Construction Spending: Power

# Removing highly correlated variables
data_num_var = data[,!names(data) %in% c("month","orders_rcvd","t")]
corr_df = cor(data_num_var)
corr_df[!lower.tri(corr_df)] = 0

simple_filter_correlated_vars = names(data_num_var[,apply(corr_df,2,function(x) any(x > 0.99))])
simple_filter_correlated_vars
length(simple_filter_correlated_vars)

uncorrelated_vars = names(data_num_var[,!apply(corr_df,2,function(x) any(x > 0.99))])
uncorrelated_vars

#corrplot.mixed(cor(data[,uncorrelated_vars]), upper="circle", lower="number")

data.new = data[,c("orders_rcvd","month", "t", uncorrelated_vars)]
#corrplot.mixed(cor(data.new[,!names(data.new) %in% c("month")]), upper="circle", lower="number")


## 2 : FCBF
library(Biocomb)

names(data)
data_reordered = as.matrix(data[ , c(4:ncol(data),1) ])
colnames(data_reordered)

for(threshold in seq(0,1, length=10)){
  fcbf_select = select.fast.filter(data_reordered, 
                                   disc.method="equal interval width", threshold=threshold, 
                                   attrs.nominal=numeric())
  
  removed_var = setdiff(colnames(data_reordered), fcbf_select$Biomarker)
  removed_var = removed_var[!grepl("orders_rcvd", removed_var)]  
  print(removed_var)
  print(paste0("#Vars Removed : ", length(removed_var)))
  
  unremoved_var = c(as.character(fcbf_select$Biomarker), "orders_rcvd")
  print(unremoved_var)
  print(paste0("#Vars UnRemoved : ", length(unremoved_var)))
}

data.new = data[,c("month", "t", unremoved_var)]


## 3 : findCorrelation
library(caret)

corr_df = cor(data_num_var)
high_corr_cols = findCorrelation(corr_df, cutoff=0.99)
findcor_correlated_cols = names(data_num_var)[high_corr_cols]
length(findcor_correlated_cols)


findcor_uncorrelated_cols = names(data_num_var)[-high_corr_cols]
length(findcor_uncorrelated_cols)

data.new = data[,c("orders_rcvd","month", "t", findcor_uncorrelated_cols)]



## Parameters
no_vars = dim(data.new)[2]/10
method = "forward" # exhaustive, forward
var_cols = names(data.new)



##### Cross-Validated Model Selection #####
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



##### Leaps' Model Selection #####
leaps = regsubsets(orders_rcvd~., data=data[,var_cols], nvmax=no_vars, method=method, really.big=TRUE)
plot(leaps)

leaps_summary = summary(leaps)

best_model = which.min(leaps_summary$bic)
coef(leaps,id=best_model)

best_model = which.max(leaps_summary$adjr2)
coef(leaps,id=best_model)

# best model with regsubsets Adj-R2 - limited to nvmax=5
# forward    : month4 + month10 + month11 + PBPWRCON + TLWSCON 
# exhaustive : PBAMUSCON + TLCADCON + TLEDUCON + TLOFCON + TLSWDCON



##### LASSO Regression #####
grid = 2.71828^seq(0.001, 9, length=1000)

x = model.matrix(orders_rcvd~., data.new)[,-1]
y = data$orders_rcvd

cv.l2.fit = cv.glmnet(x, y, alpha=1, type.measure="mae", lambda=grid)
plot(cv.l2.fit)

## best model
best_lambda = cv.l2.fit$lambda.min
best_lambda
best_lambda_index = match(best_lambda, cv.l2.fit$lambda)
best_lambda_index

cv.l2.fit$cvm[best_lambda_index]

l2.fit = glmnet(x, y, alpha=1, lambda=best_lambda)
coefs = coef(l2.fit)[,1]
coefs[coefs!=0]
names(coefs[coefs!=0])

## simpler model with 1SE
simple_lambda = cv.l2.fit$lambda.1se
simple_lambda
simple_lambda_index = match(simple_lambda, cv.l2.fit$lambda)
simple_lambda_index

cv.l2.fit$cvm[simple_lambda_index]

l2.fit = glmnet(x, y, alpha=1, lambda=simple_lambda)
coefs = coef(l2.fit)[,1]
coefs[coefs!=0]
names(coefs[coefs!=0])



## Regression Analysis
y_name = "orders_rcvd"
x = names(coefs[coefs!=0])[-1]

df = data.frame(model.matrix(~., data.new))
regression_formula = as.formula(paste0(y_name,"~",paste(x, collapse="+")))


outliers = c(37)
use_rows = setdiff(1:nrow(df), outliers)
lm_fit = lm(regression_formula, data=df, subset=use_rows)

summary(lm_fit)

# detecting leverage points - based on H
H = hatvalues(lm_fit)

H_threshold_upper = (3*(length(x)+1))/dim(df)[1]
sum(H>H_threshold_upper)
which(H>H_threshold_upper)

H_threshold_lower = (2*(length(x)+1))/dim(df)[1]
sum(H>H_threshold_lower)
which(H>H_threshold_lower)
# First is the row number

# Detecting outliers
std.res = abs(rstandard(lm_fit))
sum(std.res>=3)
which(std.res>=3)

sum(std.res>=2)
which(std.res>=2)

stu.res = abs(rstudent(lm_fit))
sum(stu.res>=3)
which(stu.res>=3)

sum(stu.res>=2)
which(stu.res>=2)


# Influential Points
cooks = cooks.distance(lm_fit)
sum(cooks>0.5)
which(cooks>0.5)

sum(cooks>1)
which(cooks>1)


# observed v fitted
plot(df$orders_rcvd[use_rows], lm_fit$fitted.values)
lines(df$orders_rcvd[use_rows], df$orders_rcvd[use_rows], col="red")

plot(lm_fit)

plot(ts(df$orders_rcvd, frequency=12))
points((outliers/12)+1, df$orders_rcvd[outliers], col="red", lwd=2)



## Bechnmarking
lm_pred = predict(lm_fit, newdata=data.new[(train_till+1):dim(data.new)[1],] )
lm_pred

getBenchmarkResults(y, lm_pred, h=h)
getBenchmarkResults(y, lm_pred, naive_model = FALSE, snaive_model = FALSE, drift_model = FALSE, h=h)




