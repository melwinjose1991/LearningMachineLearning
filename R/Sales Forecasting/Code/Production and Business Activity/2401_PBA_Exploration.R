library(corrplot)
library(leaps)

source("../Code/Utils.R")



# Loading Data
data_revenue = read.csv("../data/2401_Revenue.csv", header=TRUE, sep=",")
data_vars = read.csv("Construction.csv", header=TRUE, sep=",")

data = cbind(data_revenue[,c("orders_rcvd","month")], data_vars)
data$month = as.factor(data$month)
data = data[,!names(data) %in% c("date")]



## Correlation
corrplot.mixed(cor(data[,!names(data) %in% c("month")]), upper="circle", lower="number")
# Highest Correlation : 0.410 : PBPWRCON : Total Public Construction Spending: Power

correlation = cor(data[,!names(data) %in% c("month")])[1,]
correlation = correlation[2:length(correlation)]
related_cols = correlation[correlation>0.25]
related_cols_name = names(related_cols)

plot(data$PBPWRCON, data$orders_rcvd)
cor(data$PBPWRCON, data$orders_rcvd)

corrplot.mixed(cor(data[,related_cols_name]), upper="circle", lower="number")
related_cols_name = related_cols_name[!related_cols_name %in% c("PBPSCON","PBWSCON")]



## Fitting Models
form = as.formula(paste0("orders_rcvd ~ ",paste(related_cols_name, collapse="+")))
lr_fit = lm(form, data=data)
summary(lr_fit)
# Adj-R2 : 0.221
# f-stat : 2.394
plot(lr_fit)

form = as.formula(paste0("orders_rcvd ~ ",paste(related_cols_name, collapse="+")," + month"))
lr_fit = lm(form, data=data)
summary(lr_fit)
# Adj-R2 : 0.533
# f-stat : 2.216
plot(lr_fit)

# based on correlation
form = as.formula(paste0("orders_rcvd ~ PBPWRCON"))
lr_fit = lm(form, data=data)
summary(lr_fit)
# Adj-R2 : 0.168
# f-stat : 9.323
plot(lr_fit)

# based on SubSet Selection : Total Construction Spending: Water Supply
form = as.formula(paste0("orders_rcvd ~ TLWSCON + month"))
lr_fit = lm(form, data=data)
summary(lr_fit)
# Adj-R2 : 0.481
# f-stat : 2.711
plot(lr_fit)

form = as.formula(paste0("orders_rcvd ~ PBPWRCON + month"))
lr_fit = lm(form, data=data)
summary(lr_fit)
# Adj-R2 : 0.481
# f-stat : 2.711
plot(lr_fit)


## Cross-Validated Subset Selection
mean_errors = cvSubsetSelection(data[,c(related_cols_name,"month","orders_rcvd")])


var_cols = c(related_cols_name,"month","orders_rcvd")
leaps = regsubsets(orders_rcvd~., data=data[,var_cols], nvmax=18,
                   force.in=(length(related_cols)+1):(length(related_cols)+11) )
plot(leaps)
best_model = 1
leaps_summary = summary(leaps)
leaps_summary

leaps_summary$outmat[best_model,]
coef(leaps,id=best_model)

# FINAL : month + TLWSCON



cvSingleSeasonalModel=function(df, fixed_col_names, variable_col_names){
  #df = data
  
  #fixed_col_names = ""
  fixed_col_names = "month"
  
  #variable_col_names = related_cols_name
  #variable_col_names = names(df)[!names(df) %in% c("month","orders_rcvd")]
  
  rows = dim(df)[1]
  
  k=10
  folds = sample(1:k, rows, replace=TRUE)
  cv.errors = matrix(NA, k, length(variable_col_names))
  
  for(i in 1:k){
    for(var in variable_col_names){
        form = as.formula(paste0("orders_rcvd ~ ",fixed_col_names," + ", var))
        
        lr_fit = lm(form, df[folds!=i,])
        pred = predict(lr_fit, df[folds==i,])
        
        cv.errors[i, match(var,variable_col_names)] = mean((pred-df[folds==i,]$orders_rcvd)^2)
    }
  }
  
  mean.cv.errors = apply(cv.errors, 2, mean)
  par(mfrow=c(1,1))
  plot(mean.cv.errors, type='b')
  
  indices = sort(mean.cv.errors, method="quick", index.return=TRUE)$ix[1:5]
  print(indices)
  print(mean.cv.errors[indices])
  print(variable_col_names[indices])
  
  
}

cvSingleSeasonalModel(data, "month", related_cols_name)
cvSingleSeasonalModel(data, "month", names(df)[!names(df) %in% c("month","orders_rcvd")])

cvSingleSeasonalModel(data, "", related_cols_name)
cvSingleSeasonalModel(data, "", names(df)[!names(df) %in% c("month","orders_rcvd")])

# Mostly TLWSCON is the best single model with month