library(xgboost)



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

test_t = read.csv("xgb_final.csv", header=TRUE, sep=",")
fare_amount = test_t[,"fare_amount"]
test = cbind(test, fare_amount)

rm(train_t)
rm(test_t)



### Converting factors into numeric
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
train$vendor_id = unlist(lapply(train$vendor_id, function(x) getFromMap(as.character(x), map)))
test$vendor_id = unlist(lapply(test$vendor_id, function(x) getFromMap(as.character(x), map)))

map = getMap(train$payment_type, test$payment_type)
train$payment_type = unlist(lapply(train$payment_type, function(x) getFromMap(as.character(x), map)))
test$payment_type = unlist(lapply(test$payment_type, function(x) getFromMap(as.character(x), map)))



all_ = rbind(train[,order(names(train))], test[,order(names(test))])



### Find columns without NAs, NULLs, "", etc
hasMissing=function(df, column){
  #df = train
  #column = "TID"
  
  na_count = sum(is.na(df[,column]))
  if(na_count>0){
    print(cat("\tNAs: ", na_count))
  }
  
  null_count = sum(is.null(df[,column]))
  if(null_count>0){
    print(cat("\tNULLs: ", null_count))
  }
  
  empty_count = sum(df[,column]=="", na.rm = TRUE)
  if(empty_count>0){
    print(cat("\tEmptys: ",empty_count))
  }
  
  na_count + null_count + empty_count
  
}

cols = names(train)
non_missing_cols = vector('character')
missing_cols = vector('character')
for(col in cols ){
  print(cat("\n===",col,"==="))
  ret = hasMissing(all_, col)
  if(ret>0){
    print(cat("\t","has missing values"))
    missing_cols = c(missing_cols, col)
  }else{
    print(cat("\t","does not have missing values"))
    non_missing_cols = c(non_missing_cols, col)
  }
}

missing_cols

non_missing_cols
not_required = c("TID", "dropoff_week", "dropoff_week", "dropoff_month", "dropoff_year",
                 "pickup_datetime","dropoff_datetime")
non_missing_cols = non_missing_cols[!non_missing_cols %in% not_required]
non_missing_cols
length(non_missing_cols) # 17 both for train and all_



### Imputing for quantitative fields
imputeColumn=function(df_non_missing, df_missing, missing_col){
  ### 
  '
  missing_col = "pickup_longitude"

    # train
  df_non_missing = train[ !is.na(train[,missing_col]) & train[,missing_col]!=0 , ]
  dim(df_non_missing)[1] / dim(train)[1]
  df_missing = train[is.na(train[,missing_col]) | train[,missing_col]==0, ]
  dim(train)[1] == dim(df_missing)[1] + dim(df_non_missing)[1]
  
  ## OR 
  
  # all_
  df_non_missing = all_[ !is.na(all_[,missing_col]) & all_[,missing_col]!=0 , ]
  dim(df_non_missing)[1] / dim(all_)[1]
  df_missing = all_[is.na(all_[,missing_col]) | all_[,missing_col]==0, ]
  dim(all_)[1] == dim(df_missing)[1] + dim(df_non_missing)[1]
  '
  ###
  
  rows = dim(df_non_missing)[1]
  train_rows = sample(1:rows, 0.80*rows, replace=F)
  
  train_DM <- xgb.DMatrix(data = as.matrix(df_non_missing[train_rows, non_missing_cols]), 
                          label=df_non_missing[train_rows, missing_col])
  valid_DM <- xgb.DMatrix(data = as.matrix(df_non_missing[-train_rows, non_missing_cols]), 
                          label=df_non_missing[-train_rows, missing_col])
  
  param = list(  objective           = "reg:linear", 
                 booster             = "gbtree",
                 eta                 = 0.0125,
                 max_depth           = as.integer(length(non_missing_cols)),
                 min_child_weight    = 25,
                 subsample           = 0.8,
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
  # all_  : 0.020
  # train :
  
  test_DM = xgb.DMatrix(data=as.matrix(df_missing[,non_missing_cols]))
  predict = predict(model, test_DM)
  predict
}



## Co-ordinates
for(missing_col in c("pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude")){

  missing_exp = is.na(train[,missing_col]) | train[,missing_col]==0
  non_missing_exp = !is.na(all_[,missing_col]) & all_[,missing_col]!=0
  
  df_non_missing = all_[ non_missing_exp , ]
  df_missing = train[ missing_exp , ]
  
  train[ missing_exp, missing_col ] = imputeColumn(df_non_missing, df_missing, missing_col)
  sum(is.na(train[,missing_col]) | train[,missing_col]==0)
  sum(!is.na(train[,missing_col]) & train[,missing_col]!=0) == dim(train)[1]

}
write.csv(train[,c("TID","pickup_longitude", "pickup_latitude", "dropoff_longitude", "dropoff_latitude")], 
          "data/train_coordinates.csv", row.names = FALSE, quote = FALSE)


for(missing_col in c("pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude")){
  
  missing_exp = is.na(test[,missing_col]) | test[,missing_col]==0
  non_missing_exp = !is.na(all_[,missing_col]) & all_[,missing_col]!=0
  
  df_non_missing = all_[ non_missing_exp , ]
  df_missing = test[ missing_exp , ]
  
  test[ missing_exp, missing_col ] = imputeColumn(df_non_missing, df_missing, missing_col)
  sum(is.na(test[,missing_col]) | test[,missing_col]==0)
  sum(!is.na(test[,missing_col]) & test[,missing_col]!=0) == dim(test)[1]

}
write.csv(test[,c("TID","pickup_longitude", "pickup_latitude", "dropoff_longitude", "dropoff_latitude")], 
          "data/test_coordinates.csv", row.names = FALSE, quote = FALSE)



### surcharge
for(missing_col in c("surcharge")){
  
  missing_exp = is.na(train[,missing_col])
  non_missing_exp = !is.na(all_[,missing_col])
  
  df_non_missing = all_[ non_missing_exp , ]
  df_missing = train[ missing_exp , ]
  
  print(sum(missing_exp)) #missing entries
  
  train[ missing_exp, missing_col ] = imputeColumn(df_non_missing, df_missing, missing_col)
  sum(is.na(train[,missing_col]))
  sum(!is.na(train[,missing_col])) == dim(train)[1]
  
}
write.csv(train[,c("TID","surcharge")], "data/train_surcharge.csv", row.names = FALSE, quote = FALSE)


for(missing_col in c("surcharge")){
  
  missing_exp = is.na(test[,missing_col]) | test[,missing_col]==0
  non_missing_exp = !is.na(all_[,missing_col]) & all_[,missing_col]!=0
  
  df_non_missing = all_[ non_missing_exp , ]
  df_missing = test[ missing_exp , ]
  
  print(sum(missing_exp)) #missing entries
  
  test[ missing_exp, missing_col ] = imputeColumn(df_non_missing, df_missing, missing_col)
  sum(is.na(test[,missing_col]) | test[,missing_col]==0)
  sum(!is.na(test[,missing_col]) & test[,missing_col]!=0) == dim(test)[1]
  
}
write.csv(test[,c("TID","surcharge")], "data/test_surcharge.csv", row.names = FALSE, quote = FALSE)
