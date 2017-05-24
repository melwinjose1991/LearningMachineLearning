library(xgboost)
library(lubridate)

train = read.csv("data/train.csv", header=TRUE, sep=",")
test = read.csv("data/test.csv", header=TRUE, sep=",")

train_t = read.csv("data/train_pickup.csv", header=TRUE, sep=",")
test_t = read.csv("data/test_pickup.csv", header=TRUE, sep=",")
train = cbind(train, train_t[,!names(train_t) %in% c("TID")])
test = cbind(test, test_t[,!names(test_t) %in% c("TID")])

train_t = read.csv("data/train_dropoff.csv", header=TRUE, sep=",")
test_t = read.csv("data/test_dropoff.csv", header=TRUE, sep=",")
train = cbind(train, train_t[,!names(train_t) %in% c("TID")])
test = cbind(test, test_t[,!names(test_t) %in% c("TID")])

train_t = read.csv("data/train_time_taken.csv", header=TRUE, sep=",")
test_t = read.csv("data/test_time_taken.csv", header=TRUE, sep=",")
train = cbind(train, time_taken=train_t[,!names(train_t) %in% c("TID")])
test = cbind(test, time_taken=test_t[,!names(test_t) %in% c("TID")])



# Factorizing 
getMap=function(df1_col, df2_col){
  all_values = unique(c(unique(as.character(df1_col)),unique(as.character(df2_col))))
  map = list()
  index = 1
  print(all_values)
  for(value in all_values){
    map[value] = index
    index = index + 1
  }
  print(map)
  map
}

getFromMap=function(key, map){
  if(key %in% names(map)){
    map[[key]]
  }else{
    NA
  }
}

map = getMap(train$vendor_id, test$vendor_id)
train$vendor_id_int = unlist(lapply(train$vendor_id, function(x) getFromMap(as.character(x), map)))
test$vendor_id_int = unlist(lapply(test$vendor_id, function(x) getFromMap(as.character(x), map)))



## Rows without any NAs
if(FALSE){
  col_names = names(train)
  remove_col = c("TID", "fare_amount", "pickup_datetime", "dropoff_datetime")
  col_names = col_names[!col_names %in% remove_col]
  complete_rows_train = train[complete.cases(train[col_names]), col_names]
  dim(complete_rows_train)[1] / dim(train)[1]
  # 0.798
  
  complete_rows_test = test[complete.cases(test[col_names]), col_names]
  dim(complete_rows_test)[1] / dim(test)[1]
  # 0.796
  
  complete_rows = rbind(complete_rows_train, complete_rows_test)
  rm(complete_rows_train)
  rm(complete_rows_test)
  
  complete_rows = complete_rows[complete_rows$new_user!="",]
  
  complete_rows$vendor_id = as.numeric(unlist(complete_rows$vendor_id))
  complete_rows$new_user = as.numeric(unlist(complete_rows$new_user))
  # NO=2, YES=3
  complete_rows$payment_type = as.numeric(unlist(complete_rows$payment_type))
  complete_rows$store_and_fwd_flag = as.numeric(unlist(complete_rows$store_and_fwd_flag))
}


## Impute Missing Data
imputeCategoricalColumn=function(target_column){
  target_column = "new_user"
  target = train[train$new_user=="",]
  
  x_col = colnames(target)[colSums(is.na(target)) == 0]
  #x_col = col_names[!col_names %in% remove_col]
  x_col = intersect(x_col, names(complete_rows))
  y_col = target_column
  
  y = as.numeric(complete_rows[,target_column]) - 2
  # NO=0, YES=1
  
  rows = dim(complete_rows)[1]
  train_rows = sample(1:rows, 0.80*rows, replace=F)
  
  train_DM <- xgb.DMatrix(data = as.matrix(complete_rows[train_rows,x_col]), 
                          label=y[train_rows])
  valid_DM <- xgb.DMatrix(data = as.matrix(complete_rows[-train_rows,x_col]), 
                          label=y[-train_rows])
  
  param = list(  objective           = "multi:softprob", 
                 booster             = "gbtree",
                 eta                 = 0.0125,
                 max_depth           = as.integer(length(x)),
                 min_child_weight    = 75,
                 subsample           = 0.8,
                 colsample_bytree    = 0.60,
                 num_class           = 2
  )
  
  nrounds = 200
  model = xgb.train(   params              = param, 
                       data                = train_DM,
                       nrounds             = nrounds, 
                       early_stopping_rounds  = 20,
                       watchlist           = list(val=valid_DM),
                       maximize            = FALSE,
                       eval_metric         = "mlogloss",
                       print_every_n = 25
  )
  
  test_DM = xgb.DMatrix(data = as.matrix(target[,x_col]))
  predict = predict(model, test_DM)
}



## Vendor ID
#train$vendor_id_int = as.numeric(unlist(train[,"vendor_id"]))
#test$vendor_id_int = as.numeric(unlist(test[,"vendor_id"]))
# check `Factoring section`


## New.User
#   Some of them are "" in both train and test
sum(train$new_user=="")
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
#   check extract_date_time_variables.R



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
#train$store_and_fwd_flag_int = as.numeric(unlist(train[,"store_and_fwd_flag"]))
#test$store_and_fwd_flag_int = as.numeric(unlist(test[,"store_and_fwd_flag"]))

train[train$store_and_fwd_flag=="","store_and_fwd_flag"] = "N"
train[train$store_and_fwd_flag==" ","store_and_fwd_flag"] = "N"

test[test$store_and_fwd_flag=="","store_and_fwd_flag"] = "N"
test[test$store_and_fwd_flag==" ","store_and_fwd_flag"] = "N"

map = getMap(train$store_and_fwd_flag, test$store_and_fwd_flag)
train$store_and_fwd_flag_int = unlist(lapply(train$store_and_fwd_flag, function(x) getFromMap(as.character(x), map)))
test$store_and_fwd_flag_int = unlist(lapply(test$store_and_fwd_flag, function(x) getFromMap(as.character(x), map)))



### payment type
#train$payment_type_int = as.numeric(unlist(train[,"payment_type"]))
#test$payment_type_int = as.numeric(unlist(test[,"payment_type"]))

map = getMap(train$payment_type, test$payment_type)
train$payment_type_int = unlist(lapply(train$payment_type, function(x) getFromMap(as.character(x), map)))
test$payment_type_int = unlist(lapply(test$payment_type, function(x) getFromMap(as.character(x), map)))



### surcharge
# outliers ??? 
# negative values ???
train[is.na(train$surcharge),"surcharge"] = 0

test[is.na(test$surcharge),"surcharge"] = 0



## Extra Amount
train$extra_amt = train$tolls_amount + train$tip_amount + train$mta_tax + train$surcharge

test$extra_amt = test$tolls_amount + test$tip_amount + test$mta_tax + test$surcharge



## Difference from mean of passenger_count
calculateDifferenceFromMean=function(df1, df2, cat_column, column){
  #cat_column = "passenger_count"
  #column = "tolls_amount"
  #d1 = train
  #d2 = test
  
  all_ = rbind(df1[,c(cat_column,column)], df2[,c(cat_column,column)])
  form = as.formula(paste0(column,"~",cat_column))
  mean_value = as.data.frame(aggregate(form, all_, mean))
  rm(all_)
  
  mapply(function(category, value) value - mean_value[mean_value[,cat_column]==category, column] , 
         df1[,cat_column], df1[,column])
}

rm(mean)
train$tolls_amnt_diff_pcount = calculateDifferenceFromMean(train, test, "passenger_count", "tolls_amount")
test$tolls_amnt_diff_pcount = calculateDifferenceFromMean(test, train, "passenger_count", "tolls_amount")

train$tip_amnt_diff_pcount = calculateDifferenceFromMean(train, test, "passenger_count", "tip_amount")
test$tip_amnt_diff_pcount = calculateDifferenceFromMean(test, train, "passenger_count", "tip_amount")

train$surcharge_diff_pcount = calculateDifferenceFromMean(train, test, "passenger_count", "surcharge")
test$surcharge_diff_pcount = calculateDifferenceFromMean(test, train, "passenger_count", "surcharge")

train$time_taken_diff_pcount = calculateDifferenceFromMean(train, test, "passenger_count", "time_taken")
test$time_taken_diff_pcount = calculateDifferenceFromMean(test, train, "passenger_count", "time_taken")



## Difference from mean of payment_type_int
rm(mean)
train$tolls_amnt_diff_paytype = calculateDifferenceFromMean(train, test, "payment_type_int", "tolls_amount")
test$tolls_amnt_diff_paytype = calculateDifferenceFromMean(test, train, "payment_type_int", "tolls_amount")

train$tip_amnt_diff_paytype = calculateDifferenceFromMean(train, test, "payment_type_int", "tip_amount")
test$tip_amnt_diff_paytype = calculateDifferenceFromMean(test, train, "payment_type_int", "tip_amount")

train$surcharge_diff_paytype = calculateDifferenceFromMean(train, test, "payment_type_int", "surcharge")
test$surcharge_diff_paytype = calculateDifferenceFromMean(test, train, "payment_type_int", "surcharge")

train$time_taken_diff_paytype = calculateDifferenceFromMean(train, test, "payment_type_int", "time_taken")
test$time_taken_diff_paytype = calculateDifferenceFromMean(test, train, "payment_type_int", "time_taken")



## Difference from mean of rate_code
rm(mean)
train$tolls_amnt_diff_ratecode = calculateDifferenceFromMean(train, test, "rate_code", "tolls_amount")
test$tolls_amnt_diff_ratecode = calculateDifferenceFromMean(test, train, "rate_code", "tolls_amount")

train$tip_amnt_diff_ratecode = calculateDifferenceFromMean(train, test, "rate_code", "tip_amount")
test$tip_amnt_diff_ratecode = calculateDifferenceFromMean(test, train, "rate_code", "tip_amount")

train$surcharge_diff_ratecode = calculateDifferenceFromMean(train, test, "rate_code", "surcharge")
test$surcharge_diff_ratecode = calculateDifferenceFromMean(test, train, "rate_code", "surcharge")

train$time_taken_diff_ratecode = calculateDifferenceFromMean(train, test, "rate_code", "time_taken")
test$time_taken_diff_ratecode = calculateDifferenceFromMean(test, train, "rate_code", "time_taken")



## TODO
# tranform x
# outliers



## Factors
x = c("vendor_id_int", 
      "new_user_int", 
      "tolls_amount", 
      "tip_amount", 
      "mta_tax", 
      "time_taken", 
      "passenger_count", 
      
      "pickup_min",
      "pickup_hour",
      "pickup_yday",
      "pickup_week", 
      "pickup_month",
      "pickup_year",
      
      "dropoff_min",
      "dropoff_hour",
      "dropoff_yday",
      "dropoff_week", 
      "pickup_month",
      "dropoff_year",
      
      "rate_code",
      "store_and_fwd_flag_int",
      "payment_type_int", 
      "surcharge", 
      "distance", 
      "velocity",
      "extra_amt",
      
      "tolls_amnt_diff_pcount", 
      "tip_amnt_diff_pcount",
      "surcharge_diff_pcount", 
      "time_taken_diff_pcount",
      
      "tolls_amnt_diff_paytype", 
      "tip_amnt_diff_paytype",
      "surcharge_diff_paytype", 
      "time_taken_diff_paytype",
      
      "tolls_amnt_diff_ratecode", 
      "tip_amnt_diff_ratecode",
      "surcharge_diff_ratecode", 
      "time_taken_diff_ratecode"
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
imp


## Test Prediction
test_pred = predict(model, test_DM)
pred = data.frame("TID"=test$TID, "fare_amount"=test_pred)

file_name = paste0("xgb_", "x", toString(length(x)), "_r",nrounds, ".csv")
write.csv(pred, file_name, row.names = FALSE)

