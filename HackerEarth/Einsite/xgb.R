library(xgboost)
library(lubridate)

train = read.csv("data/train.csv", header=TRUE, sep=",")
test = read.csv("data/test.csv", header=TRUE, sep=",")

# Reading the pre-computed features
train_2 = read.csv("data/train_2.csv", header=TRUE, sep=",")
test_2 = read.csv("data/test_2.csv", header=TRUE, sep=",")



## Vendor ID
train$vendor_id_int = as.numeric(unlist(train[,"vendor_id"]))

test$vendor_id_int = as.numeric(unlist(test[,"vendor_id"]))



## New.User
#   Some of them are "" in both train and test
train[train$new_user=="","new_user"] = "NO"
train$new_user_int = as.numeric(unlist(train[,"new_user"]))

test[test$new_user=="","new_user"] = "NO"
test$new_user_int = as.numeric(unlist(test[,"new_user"]))
# YES = 3, NO=2



## toll_price
# Outliers ???



## tip_amount
# Outliers ???
# NA ? 0?
train[is.na(train$tip_amount),"tip_amount"] = 0

test[is.na(test$tip_amount),"tip_amount"] = 0



## mta_tax
# negative tax ???
# Outliers ???


## time taken
# Outliers ???
getTimeDifference=function(t1_factor, t2_factor){
  t1_str = as.character(t1_factor)
  t2_Str = as.character(t2_factor)
  #print(t1_str)
  #print(t2_Str)
  
  t1 = strptime(t1_str,"%Y-%m-%d %H:%M:%S", tz="EST")
  t2 = strptime(t2_Str,"%Y-%m-%d %H:%M:%S", tz="EST")
  #print(t1)
  #print(t2)
  
  as.numeric(difftime(t2, t1, units="secs"))
}

if(FALSE){
  train$time_taken = mapply(getTimeDifference, train$pickup_datetime, train$dropoff_datetime)
  train[train$time_taken<0,]$time_taken = 0   # should the values be swapped ?
  train_2 = data.frame(TID=train$TID, time_taken=train$time_taken)

  test$time_taken = mapply(getTimeDifference, test$pickup_datetime, test$dropoff_datetime)
  test[test$time_taken<0,]$time_taken = 0     # should the values be swapped ?
  test_2 = data.frame(TID=test$TID, time_taken=test$time_taken)
  
}else{
  train$time_taken = train_2$time_taken
  test$time_taken = test_2$time_taken
}



# Hour of day
getPartofTime=function(t_factor, func){
  #t_factor = train[20,]$pickup_datetime
  t_str = as.character(t_factor)
  t = strptime(t_str,"%Y-%m-%d %H:%M:%S", tz="EST")
  func(t)
}

if(FALSE){
  pickup_hour = unlist(lapply(train$pickup_datetime, FUN=function(x) getPartofTime(x, hour)))
  train_2 = cbind(train_2, pickup_hour)
  
  pickup_hour = unlist(lapply(test$pickup_datetime, FUN=function(x) getPartofTime(x, hour)))
  test_2 = cbind(test_2, pickup_hour)
}else{
  train$pickup_hour = train_2$pickup_hour
  test$pickup_hour = test_2$pickup_hour
}



### Distance
calculateDistance=function(x1,y1,x2,y2){
    sqrt((x1-x2)^2 + (y1-y2)^2)
}

mean_pickup_lat = mean( c(train[!is.na(train$pickup_latitude),"pickup_latitude"],  
                        test[!is.na(test$pickup_latitude),"pickup_latitude"]) )
train[is.na(train$pickup_latitude),"pickup_latitude"] = mean_pickup_lat
test[is.na(test$pickup_latitude),"pickup_latitude"] = mean_pickup_lat

mean_pickup_lon = mean( c(train[!is.na(train$pickup_longitude),"pickup_longitude"],  
                          test[!is.na(test$pickup_longitude),"pickup_longitude"]) )
train[is.na(train$pickup_longitude),"pickup_longitude"] = mean_pickup_lon
test[is.na(test$pickup_longitude),"pickup_longitude"] = mean_pickup_lon

mean_dropoff_lat = mean( c(train[!is.na(train$dropoff_latitude),"dropoff_latitude"],  
                          test[!is.na(test$dropoff_latitude),"dropoff_latitude"]) )
train[is.na(train$dropoff_latitude),"dropoff_latitude"] = mean_dropoff_lat
test[is.na(test$dropoff_latitude),"dropoff_latitude"] = mean_dropoff_lat

mean_dropoff_lon = mean( c(train[!is.na(train$dropoff_longitude),"dropoff_longitude"],  
                          test[!is.na(test$dropoff_longitude),"dropoff_longitude"]) )
train[is.na(train$dropoff_longitude),"dropoff_longitude"] = mean_dropoff_lon
test[is.na(test$dropoff_longitude),"dropoff_longitude"] = mean_dropoff_lon

train$distance = mapply(calculateDistance, train$pickup_latitude, train$pickup_longitude, 
                        train$dropoff_latitude, train$dropoff_longitude)

test$distance = mapply(calculateDistance, test$pickup_latitude, test$pickup_longitude, 
                        test$dropoff_latitude, test$dropoff_longitude)



### rate_code
# code | mean   | <0.25   | count   | >99.75    | count
# 1      14.09    3.50      3036      62.10       4047
# 2      63.24    52.50     67        78.33       79
# 3      84.78    20        
  


### payment type
# check pivot_tables with other variables for percentage of tip for fare_amount
train$payment_type_int = as.numeric(unlist(train[,"payment_type"]))

test$payment_type_int = as.numeric(unlist(test[,"payment_type"]))



### surcharge
# outliers ??? 
# negative values ???
train[is.na(train$surcharge),"surcharge"] = 0

test[is.na(test$surcharge),"surcharge"] = 0



## Saving Results of expensive operations
#     time_taken, pickup_hour
if(FALSE){
  write.csv(train_2, "data/train_2.csv", row.names=FALSE, quote=FALSE)
  write.csv(test_2, "data/test_2.csv", row.names=FALSE, quote=FALSE)
}


## Factors
x = c("vendor_id_int", "new_user_int", "tolls_amount", "tip_amount", 
      "mta_tax", "time_taken", "passenger_count", "pickup_hour", 
      "payment_type_int", "surcharge", "distance"
      )

y = c("fare_amount")

train_DM <- xgb.DMatrix(data = as.matrix(train[,x]), label=train[,y])



## Parameter Tunning
for(param_1 in c(25,50,100,200)){
  for(param_2 in c(1,2,3)){
    
    print(paste0("param1:", param_1, " and ", "param2:", param_2))
    
    param = list(  objective           = "reg:linear", 
                   booster             = "gbtree",
                   eta                 = 0.05, #0.025,
                   max_depth           = as.integer(length(x)/param_2),
                   min_child_weight    = param_1,
                   subsample           = 0.8
                   #colsample_bytree    = 0.50
    )
    
    nrounds = 200
    model = xgb.cv(      params              = param, 
                         data                = train_DM,
                         nrounds             = nrounds, 
                         nfold               = 5,
                         early_stopping_rounds  = 20,
                         #watchlist           = list(val=train_DM),
                         maximize            = FALSE,
                         eval_metric         = "mae",
                         verbose = FALSE
                         #print_every_n = 25
    )
    print(model$evaluation_log[model$best_iteration]$test_mae_mean)
  }
}
# nrounds = 200, eta = 0.05, subsample = 0.80
#   depth = length(x), min_child_weight = 50 => 1.71 
#   



## Training 
rows = dim(train)[1]
train_rows = sample(1:rows, 0.80*rows, replace=F)

train_DM <- xgb.DMatrix(data = as.matrix(train[train_rows,x]), label=train[train_rows,y])
valid_DM <- xgb.DMatrix(data = as.matrix(train[-train_rows,x]), label=train[-train_rows,y])
test_DM <- xgb.DMatrix(data = as.matrix(test[,x]))

param = list(  objective           = "reg:linear", 
               booster             = "gbtree",
               eta                 = 0.025,
               max_depth           = as.integer(length(x)),
               min_child_weight    = 50,
               subsample           = 0.8
               #colsample_bytree    = 0.50
)

nrounds = 800
model = xgb.train(   params              = param, 
                     data                = train_DM,
                     nrounds             = nrounds, 
                     early_stopping_rounds  = 20,
                     watchlist           = list(val=valid_DM),
                     maximize            = FALSE,
                     eval_metric         = "mae",
                     print_every_n = 25
)
# valid_DM mae = 1.54   -   98.37
# valid_DM mae = 0.92   -   98.37



## Test Prediction
test_pred = predict(model, test_DM)
pred = data.frame("TID"=test$TID, "fare_amount"=test_pred)

file_name = paste0("xgb_", "x", toString(length(x)), "_r",nrounds, ".csv")
write.csv(pred, file_name, row.names = FALSE)
