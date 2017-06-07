library(corrplot)
library(leaps)

source("../Common/Utils.R")



# Loading Data
data_revenue = read.csv("../../Data/2401_Revenue.csv", header=TRUE, sep=",")
data_vars = read.csv("../../Data/Production and Business Activity/Construction.csv", header=TRUE, sep=",")

data = cbind(data_revenue[,c("orders_rcvd","month")], data_vars)
data$month = as.factor(data$month)
data = data[,!names(data) %in% c("date")]



## Correlation
corrplot.mixed(cor(data[,!names(data) %in% c("month")]), upper="circle", lower="number")
# Highest Correlation : 0.410 : PBPWRCON : Total Public Construction Spending: Power

# Removing highly correlated variables
tmp = cor(data[,!names(data) %in% c("month")])
tmp[!lower.tri(tmp)] = 0
data.new = data[,!apply(tmp,2,function(x) any(x > 0.99))]
names(data.new)

if(FALSE){
  correlation = cor(data[,!names(data) %in% c("month")])[1,]
  correlation = correlation[2:length(correlation)]
  related_cols = correlation[correlation>0.25]
  related_cols_name = names(related_cols)
  related_cols_name
  
  corrplot.mixed(cor(data[,related_cols_name]), upper="circle", lower="number")
  related_cols_name = related_cols_name[!related_cols_name %in% c("PBPSCON","PBWSCON")]
}



## Cross-Validated Subset Selection
mean_errors = cvSubsetSelection(data.new)
## TO DO : re-write the above function


## Forward Selection
var_cols = names(data.new)
leaps = regsubsets(orders_rcvd~., data=data[,var_cols], nvmax=length(var_cols), method="forward", 
                   force.in=1:11)
leaps = regsubsets(orders_rcvd~., data=data[,var_cols], nvmax=length(var_cols), method="forward")
plot(leaps)

leaps_summary = summary(leaps)
leaps_summary

best_model = which.min(leaps_summary$bic)
leaps_summary$outmat[best_model,]
coef(leaps,id=best_model)

# FINAL : month + TLWSCON