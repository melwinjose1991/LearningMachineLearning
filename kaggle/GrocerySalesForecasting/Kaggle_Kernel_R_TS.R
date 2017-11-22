library(date)
library(dplyr)
library(foreach)
library(forecast)
library(reshape2)
library(lubridate)
library(data.table)
library(doParallel)



## Reading data
train = fread('data/train.csv')
test = fread('data/test.csv')

## Removing used used columns to save space
train[, id := NULL]
train[, onpromotion := NULL]
test[, onpromotion := NULL]
gc()

## Converting date to Date
train$date = as.Date(parse_date_time(train$date, '%y-%m-%d'))
test$date = as.Date(parse_date_time(test$date, '%y-%m-%d'))

## store_item_nbr = <store_nbr>_<item_nbr>
train$store_item_nbr = paste(train$store_nbr, train$item_nbr, sep="_")
train[, item_nbr:=NULL]
train[, store_nbr:=NULL]
gc()
test$store_item_nbr = paste(test$store_nbr, test$item_nbr, sep="_")
test[, item_nbr:=NULL]
test[, store_nbr:=NULL]
gc()

## filtering rows after 2017-04-01
train_sub = train[date >= as.Date("2017-04-01"), ]
train_sub = train_sub[, c('date','store_item_nbr', 'unit_sales')]
head(train_sub)
head(test)

rm(train)
gc()



# transform to log1p
train_sub$unit_sales = as.numeric(train_sub$unit_sales)
train_sub$unit_sales[train_sub$unit_sales < 0] = 0
train_sub$unit_sales = log1p(train_sub$unit_sales)

# dcast the data from long to wide format for time series forecasting
train_sub_wide = dcast(train_sub, store_item_nbr ~ date, value.var = "unit_sales", fill = 0)
head(train_sub_wide)

train_ts = ts(train_sub_wide, frequency = 7) # considering one week as a short shopping cycle

fcst_intv = 16  # 16 days of forecast interval (Aug 16 ~ 31) per the submission requirement
fcst_matrix = matrix(NA, nrow=nrow(train_ts), ncol=fcst_intv)


# foreach %dopar% : parallel processing in ETS forecasting
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

fcst_matrix = foreach(i=1:nrow(train_ts), .combine=rbind, .packages=c("forecast")) %dopar% { 
  fcst_matrix = forecast(ets(train_ts[i,]), h=fcst_intv)$mean
}
head(fcst_matrix)

# post-processing the forecast table
fcst_matrix[fcst_matrix < 0] = 0
colnames(fcst_matrix) = as.character(seq(from = as.Date("2017-08-16"), 
                                         to = as.Date("2017-08-31"), 
                                         by = 'day'))
fcst_df = as.data.frame(cbind(train_sub_wide[,1], fcst_matrix)) 
colnames(fcst_df)[1] = "store_item_nbr"

# melt the forecast data frame from wide to long format for final submission
fcst_df_long = melt(fcst_df, id = 'store_item_nbr', 
                    variable.name = "fcst_date", 
                    value.name = 'unit_sales')
fcst_df_long$store_item_nbr = as.character(fcst_df_long$store_item_nbr)
fcst_df_long$fcst_date = as.Date(parse_date_time(fcst_df_long$fcst_date,'%y-%m-%d'))
fcst_df_long$unit_sales = as.numeric(fcst_df_long$unit_sales)

# transform back to exp1p
fcst_df_long$unit_sales = expm1(fcst_df_long$unit_sales)



# generate the final submission file
submission = left_join(test, fcst_df_long, 
                        c("store_item_nbr" = "store_item_nbr", 'date' = 'fcst_date'))
submission$unit_sales[is.na(submission$unit_sales)] = 0
head(submission)

write.csv(submission[,c("id","unit_sales")], "submission_v0.csv", row.names = FALSE)

