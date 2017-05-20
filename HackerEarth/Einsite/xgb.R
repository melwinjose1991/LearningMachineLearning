library(xgboost)
library(lubridate)

train = read.csv("data/train.csv", header=TRUE, sep=",")
test = read.csv("data/test.csv", header=TRUE, sep=",")

# Reading the pre-computed features
LOAD_SAVED=TRUE
if(LOAD_SAVED){
  train_2 = read.csv("data/train_2.csv", header=TRUE, sep=",")
  test_2 = read.csv("data/test_2.csv", header=TRUE, sep=",")
}


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
# quantile(c(train$tolls_amount, test$tolls_amount), probs=seq(0,1,by=0.00125))
train[train$tolls_amount<0,"tolls_amount"] = 0

test[test$tolls_amount<0,"tolls_amount"] = 0



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

if(!LOAD_SAVED){
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
getPOSIXTime=function(t_factor){
  #t_factor = train[20,]$pickup_datetime
  t_str = as.character(t_factor)
  t = strptime(t_str,"%Y-%m-%d %H:%M:%S", tz="EST")
  t
}

getPartofTime=function(t_factor, func){
  t_factor = train[20,]$pickup_datetime
  t_str = as.character(t_factor)
  t = strptime(t_str,"%Y-%m-%d %H:%M:%S", tz="EST")
  func(t)
}

if(!LOAD_SAVED){
  
  pickup_hour = unlist(lapply(train$pickup_datetime, FUN=function(x) getPartofTime(x, hour)))
  train_2 = cbind(train_2, pickup_hour)
  
  pickup_yday = unlist(lapply(train$pickup_datetime, FUN=function(x) getPartofTime(x, yday)))
  train_2 = cbind(train_2, pickup_yday)
  
  pickup_week = unlist(lapply(train$pickup_datetime, FUN=function(x) getPartofTime(x, week)))
  train_2 = cbind(train_2, pickup_week)
  
  pickup_month = unlist(lapply(train$pickup_datetime, FUN=function(x) getPartofTime(x, month)))
  train_2 = cbind(train_2, pickup_month)
  
  
  pickup_hour = unlist(lapply(test$pickup_datetime, FUN=function(x) getPartofTime(x, hour)))
  test_2 = cbind(test_2, pickup_hour)
  
  pickup_yday = unlist(lapply(test$pickup_datetime, FUN=function(x) getPartofTime(x, yday)))
  test_2 = cbind(test_2, pickup_yday)
  
  pickup_week = unlist(lapply(test$pickup_datetime, FUN=function(x) getPartofTime(x, week)))
  test_2 = cbind(test_2, pickup_week)
  
  pickup_month = unlist(lapply(test$pickup_datetime, FUN=function(x) getPartofTime(x, hour)))
  test_2 = cbind(test_2, pickup_month)

}else{
  
  train$pickup_hour = train_2$pickup_hour
  train$pickup_yday = train_2$pickup_yday
  train$pickup_week = train_2$pickup_week
  train$pickup_month = train_2$pickup_month
  
  test$pickup_hour = test_2$pickup_hour
  test$pickup_yday = test_2$pickup_yday
  test$pickup_week = test_2$pickup_week
  test$pickup_month = test_2$pickup_month

}




### Distance
calculateDistance=function(x1,y1,x2,y2){
  if(is.na(x1) | x1==0 | is.na(y1) | y1==0 | is.na(x2) | x2==0 | is.na(y2) | y2==0 ){
    0
  }else{ 
    sqrt((x1-x2)^2 + (y1-y2)^2)
  }
}

train$distance = mapply(calculateDistance, train$pickup_latitude, train$pickup_longitude, 
                        train$dropoff_latitude, train$dropoff_longitude)

test$distance = mapply(calculateDistance, test$pickup_latitude, test$pickup_longitude, 
                       test$dropoff_latitude, test$dropoff_longitude)

mean_distance = mean(c(train[train$distance>0,"distance"], test[test$distance>0,"distance"]))

train[train$distance<=0,"distance"] = mean_distance
test[test$distance<=0,"distance"] = mean_distance



### Velocity
getVelocity = function(dist, time){
  if(time==0){
    0
  }else{
    dist/time
  }
}
train$velocity = mapply(getVelocity, train$distance, train$time_taken)
test$velocity = mapply(getVelocity, test$distance, test$time_taken)

mean_velocity = mean(c(train[train$velocity>0,"velocity"], test[test$velocity>0,"velocity"]))

train[train$velocity<=0,"velocity"] = mean_velocity
test[test$velocity<=0,"velocity"] = mean_velocity



### rate_code
# ???
  

### store_and_fwd 
train$store_and_fwd_flag_int = as.numeric(unlist(train[,"store_and_fwd_flag"]))

test$store_and_fwd_flag_int = as.numeric(unlist(test[,"store_and_fwd_flag"]))



### payment type
# check pivot_tables with other variables for percentage of tip for fare_amount
train$payment_type_int = as.numeric(unlist(train[,"payment_type"]))

test$payment_type_int = as.numeric(unlist(test[,"payment_type"]))



### surcharge
# outliers ??? 
# negative values ???
train[is.na(train$surcharge),"surcharge"] = 0

test[is.na(test$surcharge),"surcharge"] = 0



## Extra Amount
train$extra_amt = train$tolls_amount + train$tip_amount + train$mta_tax + train$surcharge

test$extra_amt = test$tolls_amount + test$tip_amount + test$mta_tax + test$surcharge



## Difference from mean of passenger_count
calculateDifferenceFromMean=function(df1, df2, column){
  #column = "tolls_amount"
  all_ = rbind(df1[,c("passenger_count",column)], df2[,c("passenger_count",column)])
  form = as.formula(paste0(column,"~passenger_count"))
  mean_value = as.data.frame(aggregate(form, all_, mean))
  rm(all_)
  
  mapply(function(passenger_count, value) value - mean_value[mean_value$passenger_count==passenger_count, column] , 
         df1$passenger_count, df1[,column])
}

rm(mean)
train$tolls_amnt_diff_pcount = calculateDifferenceFromMean(train, test, "tolls_amount")
test$tolls_amnt_diff_pcount = calculateDifferenceFromMean(test, train, "tolls_amount")

train$tip_amnt_diff_pcount = calculateDifferenceFromMean(train, test, "tip_amount")
test$tip_amnt_diff_pcount = calculateDifferenceFromMean(test, train, "tip_amount")

#train$surcharge_diff_pcount = calculateDifferenceFromMean(train, test, "surcharge")
#test$surcharge_diff_pcount = calculateDifferenceFromMean(test, train, "surcharge")

train$time_taken_diff_pcount = calculateDifferenceFromMean(train, test, "time_taken")
test$time_taken_diff_pcount = calculateDifferenceFromMean(test, train, "time_taken")



## Saving Results of expensive operations
#     time_taken, pickup_hour
if(!LOAD_SAVED){
  write.csv(train_2, "data/train_2.csv", row.names=FALSE, quote=FALSE)
  write.csv(test_2, "data/test_2.csv", row.names=FALSE, quote=FALSE)
}


## TODO
# tranform x
# outliers

## Factors
x = c("vendor_id_int", 
      #"new_user_int", 
      "tolls_amount", 
      "tip_amount", 
      "mta_tax", 
      "time_taken", 
      #"passenger_count", 
      "pickup_hour",
      "pickup_yday",
      "pickup_week", 
      #"pickup_month", 
      "rate_code",
      "store_and_fwd_flag_int",
      "payment_type_int", 
      #"surcharge", 
      "distance", 
      "velocity",
      "extra_amt",
      
      "tolls_amnt_diff_pcount", 
      "tip_amnt_diff_pcount",
      #"surcharge_diff_pcount", 
      "time_taken_diff_pcount"
      )

y = c("fare_amount")

rows = dim(train)[1]
train_rows = sample(1:rows, 0.80*rows, replace=F)

train_DM <- xgb.DMatrix(data = as.matrix(train[train_rows,x]), label=train[train_rows,y])
valid_DM <- xgb.DMatrix(data = as.matrix(train[-train_rows,x]), label=train[-train_rows,y])



## Parameter Tunning
for(param_1 in c(25,50,75,125,150,175)){                # min_child_weight
  for(param_2 in c(1)){                 # max_depth divider
    for(param_3 in c(0.8)){     # subsample
      for(param_4 in c(0.6)){   # colsample_bytree
        
        print(paste0("param1:", param_1, " and ", "param2:", param_2, 
                     " and ", "param3:", param_3," and ", "param4:", param_4))
        
        param = list(  objective           = "reg:linear", 
                       booster             = "gbtree",
                       eta                 = 0.5, #0.025,
                       max_depth           = as.integer(length(x)/param_2),
                       min_child_weight    = param_1,
                       subsample           = param_3,
                       colsample_bytree    = param_4
                       #alpha               = param_3
                       #lambda              = param_4
        )
        
        nrounds = 200
        model = xgb.cv(      params              = param, 
                             data                = train_DM,
                             nrounds             = nrounds, 
                             nfold               = 4,
                             early_stopping_rounds  = 20,
                             watchlist           = list(val=valid_DM),
                             maximize            = FALSE,
                             eval_metric         = "mae",
                             verbose             = FALSE
                             #print_every_n       = 50
        )
        print(model$evaluation_log[model$best_iteration]$test_mae_mean)
      
      }
    }
  }
}
# nrounds = 200, eta = 0.5
#   depth   min_child_weight  alpha   lambda  sample  colSample 
#   1       50              



## Training 
rows = dim(train)[1]
train_rows = sample(1:rows, 0.80*rows, replace=F)

train_DM <- xgb.DMatrix(data = as.matrix(train[train_rows,x]), label=train[train_rows,y])
valid_DM <- xgb.DMatrix(data = as.matrix(train[-train_rows,x]), label=train[-train_rows,y])
test_DM <- xgb.DMatrix(data = as.matrix(test[,x]))

param = list(  objective           = "reg:linear", 
               booster             = "gbtree",
               eta                 = 0.0125,
               max_depth           = as.integer(length(x)),
               min_child_weight    = 75,
               subsample           = 0.8,
               #alpha               = 0.005
               colsample_bytree    = 0.60
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
# valid_DM mae = 0.92   -   99.03
# valid_DM mae = 0.90   -   99.05
# valid_DM mae = 0.88   -   99.06
# valid_DM mae = 0.80   -   99.13   (7.70, 4.14, 2.28, 1.38, 1.01)
imp = xgb.importance(feature_names = x, model = model)



## Test Prediction
test_pred = predict(model, test_DM)
pred = data.frame("TID"=test$TID, "fare_amount"=test_pred)

file_name = paste0("xgb_", "x", toString(length(x)), "_r",nrounds, ".csv")
write.csv(pred, file_name, row.names = FALSE)

