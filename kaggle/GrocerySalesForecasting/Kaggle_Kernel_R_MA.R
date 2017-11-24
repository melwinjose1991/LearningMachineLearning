library(date)
library(dplyr)
library(foreach)
library(forecast)
library(reshape2)
library(lubridate)
library(data.table)
library(doParallel)


rm(list=tables()$NAME)
memory.limit(size=memory.limit()*2)
gc()


## Reading data
train = fread('data/train.csv')
test = fread('data/test.csv')
holiday = fread('data/holidays_events.csv')


## Removing used used columns to save space
train[, id := NULL]
gc()


## Converting date to Date
train$date = as.Date(parse_date_time(train$date, '%y-%m-%d'))
test$date = as.Date(parse_date_time(test$date, '%y-%m-%d'))
train = train[date >= as.Date("2017-04-01"), ]
test$dow = wday(test$date)
gc()


## Transformation
holiday = holiday[transferred==FALSE]
train$unit_sales = as.numeric(train$unit_sales)
train$unit_sales[train$unit_sales < 0] = 0
train$unit_sales = log1p(train$unit_sales)


## store_item_nbr = <store_nbr>_<item_nbr>
train$store_item_nbr = paste(train$store_nbr, train$item_nbr, sep="_")
train[, item_nbr:=NULL]
train[, store_nbr:=NULL]
gc()
test$store_item_nbr = paste(test$store_nbr, test$item_nbr, sep="_")
test[, item_nbr:=NULL]
test[, store_nbr:=NULL]
gc()


## dcast the data from long to wide format for time series forecasting
train_wide = dcast(train, store_item_nbr~date, value.var="unit_sales", fill=0)
head(train_wide)
dim(train_wide)

train = melt(train_wide, id = 'store_item_nbr', 
                  variable.name = "date", 
                  value.name = 'unit_sales')
head(train)
dim(train)[1] == (dim(train_wide)[1] * (dim(train_wide)[2]-1))
dim(train)

train$dow = wday(train$date)
head(train)

rm(train_wide)
gc()


## Days of weeks means
ma_dw = train[, madw:=mean(unit_sales), by=list(store_item_nbr, dow)]
ma_wk = ma_dw[, mawk:=mean(madw), by=list(store_item_nbr)]
