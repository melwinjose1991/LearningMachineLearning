library(FNN)
library(rknn)

source("../Common/Utils.R")


## Config file
config_data = read.csv("../../Config/2401/external_data_config.csv", header=TRUE, sep=",")
config_data = config_data[config_data$explore=="yes",]



## Parameters
data_folder = "../../Data/2401/"
revenue_file = paste0(data_folder,"2401_Revenue.csv")
sa_OR_nsa = "Not Seasonally Adjusted"



# Loading Data
data_revenue = read.csv(revenue_file, header=TRUE, sep=",")
data = data_revenue[,c("orders_rcvd","month","t")]
data$month = as.factor(data$month)

for(sub_category_id in config_data$sub_category_id){
  
  category_name = config_data[config_data$sub_category_id==sub_category_id,"category_name"]
  sub_category_name = config_data[config_data$sub_category_id==sub_category_id,"sub_category_name"]
  
  
  file = paste0(data_folder ,as.character(category_name), "/", as.character(sub_category_name))
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


# Scaling 
data_scaled = scale(data[,!names(data) %in% c("orders_rcvd","month")])
colMeans(data_scaled)
apply(data_scaled, 2, sd)



# KNN - Backward Elimination Feature Selection with Random KNN
# https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-12-450
backward_KNN = rknnBeg(data_scaled, data$orders_rcvd, k=6)
plot(backward_KNN)
best_features = backward_KNN$p



# KNN-Regression
k=5
neighbors = 1:10
cv.errors = matrix(NA, k, length(neighbors))

for(neighbor in neighbors){

  folds = sample(1:k, nrow(data_scaled), replace=TRUE)
  
  for(i in 1:k){
    train = data_scaled[folds!=i, best_features]
    valid = data_scaled[folds==i, best_features]
    model = FNN::knn.reg(train, test=valid, y=data$orders_rcvd[folds!=i], k=neighbor, algorithm="kd_tree")
    cv.errors[i, neighbor] = mean(abs(model$pred - data$orders_rcvd[folds==i]))
  }
}
cv.errors

mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors

best_index = which.min(mean.cv.errors)
best_index
mean.cv.errors[best_index]
# without feature selection - 3493
# with feature selection - 3067


## Evaluation against benchmark model
h = 3
train_till = dim(data.new)[1] - h
lm_fit = lm(orders_rcvd~PBPWRCON, data=data.new[1:train_till,])
lm_pred = predict(lm_fit, newdata=data.new[(train_till+1):dim(data.new)[1],] )
lm_pred

getBenchmarkResults(y, lm_pred, h=h)
getBenchmarkResults(y, lm_pred, naive_model = FALSE, snaive_model = FALSE, drift_model = FALSE, h=h)

