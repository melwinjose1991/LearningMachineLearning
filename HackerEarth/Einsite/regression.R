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

rm(train_t)
rm(test_t)



## Vendor ID - DONE
#levels(train$vendor_id)
#plot(train[train$fare_amount<70,"vendor_id"], train[train$fare_amount<70,"fare_amount"])
#quantile(train$fare_amount, probs=seq(0,1,by=0.00125))



## New.User - DONE
#   Some of them are "" in both train and test
train[train$new_user=="","new_user"] = "NO"
train$new_user = as.factor(train$new_user)

test[test$new_user=="","new_user"] = "NO"
test$new_user = as.factor(test$new_user)
## For new_user=YES, the fare_amount is ZERO !!!



## toll_price - DONE
# Outliers & Linear Relationship
if(FALSE){
  quantile(train$fare_amount, probs=seq(0,1,by=0.00125))
  quantile(train$tolls_amount, probs=seq(0,1,by=0.00125))
  x = train[train$tolls_amount<12 & train$tolls_amount >=0 & train$fare_amount<100 & train$fare_amount>=0,
            c("tolls_amount","fare_amount")]
  dim(x)[1] / dim(train)[1]
  # 0.998
  
  plot(x$tolls_amount, x$fare_amount)
  cor(x$tolls_amount, x$fare_amount) 
  # 0.66, cor without outliers removed is 0.529
}

train = train[train$tolls_amount<12 & train$tolls_amount >=0 & train$fare_amount<100 & train$fare_amount>=0,]

if(FALSE){
  rows = dim(train)[1]
  train_rows = sample(1:rows, 0.75*rows, replace=F)
  train_df = train[train_rows, c("tolls_amount","fare_amount")]
  valid_df = train[-train_rows, c("tolls_amount","fare_amount")]
  
  lm_toll = lm(fare_amount~tolls_amount, data=train_df)
  #summary(lm_toll)
  
  mean(abs(predict(lm_toll,newdata=valid_df)-valid_df$fare_amount))
  # 6.039
}



## tip_amount - DONE
train[is.na(train$tip_amount),"tip_amount"] = 0
test[is.na(test$tip_amount),"tip_amount"] = 0

if(FALSE){
  quantile(train$tip_amount, probs=seq(0,1,by=0.00125))
  x = train[train$tip_amount<16, c("tip_amount","fare_amount")]
  dim(x)[1] / dim(train)[1]
  # 0.999
  
  plot(x$tip_amount, x$fare_amount)
  cor(x$tip_amount, x$fare_amount) 
  # 0.63, cor without outliers removed is 0.63
}

train = train[train$tip_amount<16,]

if(FALSE){
  rows = dim(train)[1]
  train_rows = sample(1:rows, 0.75*rows, replace=F)
  train_df = train[train_rows, c("tip_amount","fare_amount")]
  valid_df = train[-train_rows, c("tip_amount","fare_amount")]
  
  lm_tip = lm(fare_amount~tip_amount, data=train_df)
  #summary(lm_tip)
  
  mean(abs(predict(lm_tip,newdata=valid_df)-valid_df$fare_amount))
  # 5.790
}



## mta_tax - DONE
train$mta_tax = as.factor(train$mta_tax)

test[test$mta_tax==10.41,"mta_tax"]=0.5
test$mta_tax = as.factor(test$mta_tax)



## time taken
#   check extract_date_time_variables.R

cor(train$fare_amount, train$time_taken)
# 0.08
if(FALSE){
  quantile(train$time_taken, probs=seq(0,1,by=0.00125))
  x = train[train$time_taken>0 & train$time_taken<5000, c("time_taken","fare_amount")]
  dim(x)[1] / dim(train)[1]
  # 0.996
  
  plot(x$time_taken, x$fare_amount)
  cor(x$time_taken, x$fare_amount) 
  # 0.838, cor without outliers removed is 0.08
}
train = train[train$time_taken>0 & train$time_taken<5000,]



# Parts of time
## <<< TO-DO >>>
train$pickup_min_c = as.factor(train$pickup_min)
test$pickup_min_c = as.factor(test$pickup_min)

train$pickup_hour_c = as.factor(train$pickup_hour)
test$pickup_hour_c = as.factor(test$pickup_hour)

train$pickup_yday_c = as.factor(train$pickup_yday)
test$pickup_yday_c = as.factor(test$pickup_yday)

train$pickup_week_c = as.factor(train$pickup_week)
test$pickup_week_c = as.factor(test$pickup_week)

train$pickup_month_c = as.factor(train$pickup_month)
test$pickup_month_c = as.factor(test$pickup_month)

if(FALSE){
  rows = dim(train)[1]
  train_rows = sample(1:rows, 0.75*rows, replace=F)
  train_df = train[train_rows, 
                   c("pickup_min_c", "pickup_hour_c", "pickup_yday_c", 
                     "pickup_week_c", "pickup_month_c", "fare_amount")]
  valid_df = train[-train_rows, 
                   c("pickup_min_c", "pickup_hour_c", "pickup_yday_c", 
                     "pickup_week_c", "pickup_month_c", "fare_amount")]
  
  lm_pickup = lm(fare_amount~pickup_hour_c, 
                 data=train_df)
  summary(lm_pickup)
  
  mean(abs(predict(lm_pickup,newdata=valid_df)-valid_df$fare_amount))
  # 7.607 = min, hour
  #       = min, hour, week, month
}


### Distance - DONE
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

if(FALSE){
  quantile(train[train$distance>0,]$distance, probs=seq(0,1,by=0.00125))
  x = train[train$distance >0 & train$distance <1,c("distance","fare_amount")]
  # 0.999
  #x = train[,c("distance","fare_amount")]
  
  plot(x$distance, x$fare_amount)
  cor(x$distance, x$fare_amount) 
  # 0.187 = just the distance
  # 0.187 = + zeros repalced with mean_distance  
  # 0.865 = + outliers removed (<0.01%)
  # 0.907 = above without mean_distance replacement
}

train = train[train$distance >0 & train$distance <1,]

if(FALSE){
  rows = dim(train)[1]
  train_rows = sample(1:rows, 0.75*rows, replace=F)
  train_df = train[train_rows, c("distance","fare_amount")]
  valid_df = train[-train_rows, c("distance","fare_amount")]
  
  lm_dist = lm(fare_amount~distance, data=train_df)
  #summary(lm_dist)
  
  mean(abs(predict(lm_dist,newdata=valid_df)-valid_df$fare_amount))
  # 7.263 without outliers removed
  # 3.306 after outliers were removed
}



### rate_code
train$rate_code_c = as.factor(train$rate_code)
test$rate_code_c = as.factor(test$rate_code)


### payment type - DONE
# check pivot_tables with other variables for percentage of tip for fare_amount



### surcharge
# can be bin-ed into 0,0.5,1 , covers 99.99%
createSurchargeBuckets = function(df){
  df$surcharge_c = "NA"
  df[df$surcharge<0.25,"surcharge_c"] = "0"
  df[df$surcharge>=0.25 & df$surcharge<0.75,"surcharge_c"] = "0.5"
  df[df$surcharge>0.75,"surcharge_c"] = "1"
  as.factor(df$surcharge_c)
}

train[is.na(train$surcharge),"surcharge"] = 0
train[train$surcharge<0,"surcharge"] = 0

test[is.na(test$surcharge),"surcharge"] = 0
train[train$surcharge<0,"surcharge"] = 0

train$surcharge_c = createSurchargeBuckets(train)
test$surcharge_c = createSurchargeBuckets(test)



## Training - lm
for(i in c(1,2,3)){
  
  print(paste0("Round#",i))
  
  rows = dim(train)[1]
  train_rows = sample(1:rows, 0.75*rows, replace=F)
  train_df = train[train_rows, ]
  valid_df = train[-train_rows, ]
  lm_all = lm( fare_amount ~ vendor_id + new_user + surcharge_c + payment_type + mta_tax +
                          tolls_amount*distance*tip_amount*time_taken, 
              data=train_df)
  #summary(lm_all)
  
  pred = predict(lm_all,newdata=valid_df)
  
  #quantile(train$fare_amount, probs=seq(0,1,by=0.00125))
  sum(pred<0)
  pred[pred<0]=0
  
  sum(pred>100)
  pred[pred>100]=100
  
  print(mean(abs(pred-valid_df$fare_amount)))
  # 5.81 =        fare_amount~vendor_id+new_user+tolls_amount+tip_amount+mta_tax
  # 2.69 =        fare_amount~vendor_id+new_user+tolls_amount+tip_amount+mta_tax+distance
  # 2.66 =        fare_amount~vendor_id+new_user+tolls_amount+tip_amount+mta_tax+distance+surcharge_c
  # 2.66 = 97.071 fare_amount~vendor_id+new_user+tolls_amount+tip_amount+mta_tax+distance+surcharge_c+payment_type
  # 2.66 =        fare_amount~vendor_id+new_user+tolls_amount+tip_amount+mta_tax+distance+surcharge_c+payment_type+time_taken
  # 2.51 = 97.246 fare_amount~vendor_id+new_user+surcharge_c+payment_type+mta_tax+ (tolls_amount*distance*tip_amount)
  # 2.48 = 97.246 fare_amount~vendor_id+new_user+surcharge_c+payment_type+mta_tax+ (tolls_amount*distance*tip_amount) + rate_code_c
  # 1.32 = 98.xxx fare_amount ~ vendor_id + new_user + surcharge_c + payment_type + mta_tax + tolls_amount*distance*tip_amount + time_taken
  # 1.27 = 98.501 fare_amount ~ vendor_id + new_user + surcharge_c + payment_type + mta_tax + tolls_amount*distance*tip_amount*time_taken
  #               (^ without extrpolation & averaging)
  
  test_pred = predict(lm_all,newdata=test)
  sum(test_pred<0)
  test_pred[test_pred<0] = 0
  
  sum(test_pred>100)
  test_pred[test_pred>100] = 100
  
  pred_i = paste0("pred_",i)
  assign(pred_i, test_pred)

}

pred = data.frame("TID"=test$TID, "fare_amount"=(pred_1+pred_2+pred_3)/3)

file_name = "lm_final.csv"
write.csv(pred, file_name, row.names=FALSE, quote=FALSE)
