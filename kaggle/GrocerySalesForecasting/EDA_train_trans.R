library(data.table)
library(ggplot2)



rm(list=tables()$NAME)
gc()



eda_train = fread("data/train.csv")
eda_test = fread("data/test.csv")

setkey(eda_train, store_nbr, item_nbr)
setkey(eda_test, store_nbr, item_nbr)

#eda_train[ , date_ := as.Date(as.character(date), format = "%Y-%m-%d") ]
eda_test[ , date_ := as.Date(as.character(date), format = "%Y-%m-%d") ]



## Utility Functions

### Plot for store_no and item_no
plotTimeSeries = function(store_no, item_no, show_year=-1){
  
  #store_no = 1
  #item_no = 108079
  
  tmp_data_table = eda_train[ .(store_no, item_no) ]
  tmp_data_table[ , c("year","month","day") := tstrsplit(date, "-", fixed=TRUE) ]
  tmp_data_table[ , date_ := as.Date(date)]
  if(show_year!=-1){
    tmp_data_table = tmp_data_table[year==show_year]
  }
  head(tmp_data_table)
  
  ggplot(tmp_data_table, aes(x=date_, y=unit_sales)) + 
    geom_line() + geom_smooth()
  
}

plotTimeSeries(1, 108079, 2014)
gc()



gc()
### Checking the number of entries for each product
eda_train[ , .(count=.N) , by=item_nbr]
# 4036 products
# not all have the same number of entries



gc()
### Checking the number of entries for each store
eda_train[,.(count=.N),by=store_nbr]
# 54 store_nbr
# not all have the same number of entries

ggplot(eda_train[sample(.N,dim(eda_train)[1]*0.10)], 
       aes(x=factor(store_nbr))) + 
  geom_bar()



### Checking if entries in test are dated after train
gc()

tmp_1 = eda_test[ , .(min_epoch=min(as.integer(date_))), by=list(item_nbr,store_nbr) ]
head(tmp_1)
dim(tmp_1)

tmp_2 = eda_train[ , .(max_epoch=max(as.integer(as.Date(as.character(date),format="%Y-%m-%d")))), 
                   by=list(item_nbr,store_nbr) ]
head(tmp_2)
dim(tmp_2)

tmp = merge(tmp_1, tmp_2, all.x=TRUE)
head(tmp)
dim(tmp)

length(tmp$max_epoch < tmp$min_epoch) == dim(tmp)[1]
# all dates in test are after the max date in train
# for each combination of store_nbr x item_nbr



### More statistics about test
unique(eda_test$date)
# 16 dates

# 54 stores vs 54 in train

# 3901 items vs 4036 in train
#   60 new in test

16 * 54 * 3901 == dim(eda_test)[1]
