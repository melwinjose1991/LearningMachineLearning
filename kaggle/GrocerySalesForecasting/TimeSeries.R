library(forecast)
library(data.table)



rm(list=tables()$NAME)
gc()



train = fread("data/train.csv")
test = fread("data/test.csv")

setkey(train, store_nbr, item_nbr)
setkey(test, store_nbr, item_nbr)



### Mean Prediction for item_nbr x store_nbr
tmp_train = train[, .(mean_sales=mean(unit_sales)), by=list(item_nbr, store_nbr)]
head(tmp_train)

tmp_test = merge(test, tmp_train[,.(item_nbr, store_nbr, mean_sales)], all.x=TRUE)
dim(tmp_test)[1] == dim(test)[1]
head(tmp_test)

sum(is.na(tmp_test$mean_sales))
tmp_test[is.na(mean_sales) | mean_sales<0, mean_sales:=0]

sum(tmp_test$mean_sales<0)
tmp_test[mean_sales<0, mean_sales:=0]


setnames(tmp_test, "mean_sales", "unit_sales")
write.table(tmp_test[,.(id, unit_sales)], "submission.csv", quote=FALSE, sep=",", row.names=FALSE)



### Mean Prediction for item_nbr
tmp_train = train[, .(mean_sales=mean(unit_sales)), by=list(item_nbr)]
setnames(tmp_train, "item_nbr", "tmp_item_nbr")
head(tmp_train)

test[, unit_sales:=tmp_train[tmp_item_nbr==item_nbr,c("mean_sales")], by=item_nbr]
head(test)

write.table(test[,.(id, unit_sales)], "submission.csv", quote=FALSE, sep=",", row.names=FALSE)



### Last value Prediction
getLastSales = function(date, sales){
  epoch = as.integer(as.Date(as.character(date),format="%Y-%m-%d"))
  sales[which.max(epoch)]
}

tmp_train = train[, .(last_sales=getLastSales(date, unit_sales)), by=list(item_nbr, store_nbr)]
head(tmp_train)

tmp_test = merge(test, tmp_train[,.(item_nbr, store_nbr, last_sales)], all.x=TRUE)
dim(tmp_test)[1] == dim(test)[1]
head(tmp_test)

sum(is.na(tmp_test$last_sales) | tmp_test<0)
tmp_test[is.na(last_sales) | last_sales<0, last_sales:=0]

setnames(tmp_test, "last_sales", "unit_sales")
write.table(tmp_test[,.(id, unit_sales)], "submission.csv", quote=FALSE, sep=",", row.names=FALSE)
