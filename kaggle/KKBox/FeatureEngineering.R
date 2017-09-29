library(data.table)



###### Removing tables() ######
rm(list=tables()$NAME)
gc()



###### reading the csv files ######
train_data = fread("data/train.csv")
test_data = fread("data/sample_submission_zero.csv")
transactions_data = fread("data/transactions.csv")



###### Utility Functions ######
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



###### nof_transcations, mode_payment_id, payment_types ######
getPercentFromTransaction = function(id, col, value, total){
  count = dim(transactions_data[msno==id & transactions_data[[col]]==value])[1]
  (100*count)/total
}

merged = merge(train_data, transactions_data[, .(msno, payment_method_id)], all.x=TRUE)
FE_train = merged[, .( mode_pay_id = Mode(payment_method_id), 
                       payment_types = length(unique(payment_method_id)),
                       is_churn = max(is_churn),
                       nof_transcations = .N
                      ), 
                  by=msno]
head(FE_train)
dim(FE_train)[1] == dim(train_data)[1]

# FE_train[[paste0("percent_41")]] = sapply(FE_train$msno, function(id){
#   getPercentFromTransaction(id, "payment_method_id", 41, FE_train[msno==id]$nof_transcations)
# })


merged = merge(test_data, transactions_data[, .(msno, payment_method_id)], all.x=TRUE)
FE_test = merged[, .( mode_pay_id=Mode(payment_method_id), 
                      payment_types=length(unique(payment_method_id)),
                      is_churn=max(is_churn),
                      nof_transcations = .N
                     ), 
                 by=msno]
head(FE_test)
dim(FE_test)[1] == dim(test_data)[1]



###### payment_plan_days, plandays_unique ######
merged = merge(train_data, transactions_data[, .(msno, payment_plan_days)], all.x=TRUE)
tmp = merged[, .(mode_plandays=Mode(payment_plan_days), 
                               plandays_unique=length(unique(payment_plan_days))), by=msno ]
head(tmp)
FE_train = merge(FE_train, tmp, by="msno", all.x=TRUE)
head(FE_train)

merged = merge(test_data, transactions_data[, .(msno, payment_plan_days)], all.x=TRUE)
tmp = merged[, .(mode_plandays=Mode(payment_plan_days), 
                               plandays_unique=length(unique(payment_plan_days))), by=msno ]
head(tmp)
FE_test = merge(FE_test, tmp, by="msno", all.x=TRUE)
head(FE_test)



###### plan_list_price & actual_amount_paid ######
getCode = function(plan_list_price, actual_amount_paid){
  if( is.na(plan_list_price) || is.na(actual_amount_paid) ){
    return(NA)
  }else if(plan_list_price > actual_amount_paid){
    return(-1)
  }else if(plan_list_price == actual_amount_paid){
    return(0)
  }else{
    return(1)
  }
}

merged = merge(train_data, 
               transactions_data[, .(msno, plan_list_price, actual_amount_paid)], 
               all.x=TRUE)
merged[, pay_code := mapply(getCode, plan_list_price, actual_amount_paid)]
tmp = merged[, .(mode_pay_code=Mode(pay_code), is_churn=max(is_churn),
                 paid_less=sum(actual_amount_paid<plan_list_price),
                 paid_equal=sum(actual_amount_paid==plan_list_price),
                 paid_more=sum(actual_amount_paid>plan_list_price)), 
             by=msno]
tmp[,is_churn:=NULL]

FE_train = merge(FE_train, tmp, by="msno", all.x=TRUE)
head(FE_train)


merged = merge(test_data, 
               transactions_data[, .(msno, plan_list_price, actual_amount_paid)], 
               all.x=TRUE)
merged[, pay_code := mapply(getCode, plan_list_price, actual_amount_paid)]
tmp = merged[, .(mode_pay_code=Mode(pay_code), is_churn=max(is_churn),
                 paid_less=sum(actual_amount_paid<plan_list_price),
                 paid_equal=sum(actual_amount_paid==plan_list_price),
                 paid_more=sum(actual_amount_paid>plan_list_price)), 
             by=msno]
tmp[,is_churn:=NULL]
FE_test = merge(FE_test, tmp, by="msno", all.x=TRUE)
head(FE_test)



###### mode(is_auto_renew), %auto_renew, ######
merged = merge(train_data, transactions_data[, .(msno, is_auto_renew)], all.x=TRUE)
tmp = merged[, .( mode_auto_renew = Mode(is_auto_renew), 
                     auto_renews = sum(is_auto_renew==1)
                   ), 
                by=msno ]
FE_train = merge(FE_train, tmp, by="msno", all.x=TRUE)
FE_train[, percent_auto_renews := (100*auto_renews)/nof_transcations]
head(FE_train)


merged = merge(test_data, transactions_data[, .(msno, is_auto_renew)], all.x=TRUE)
tmp = merged[, .( mode_auto_renew = Mode(is_auto_renew), 
                     auto_renews = sum(is_auto_renew==1)
                   ), 
                by=msno ]
FE_test = merge(FE_test, tmp, by="msno", all.x=TRUE)
FE_test[, percent_auto_renews := (100*auto_renews)/nof_transcations]
head(FE_test)



###### Write train and test ######
head(FE_train)
write.table(FE_train, "data/fe_train_1.csv", quote=FALSE, sep=",", row.names=FALSE)

head(FE_test)
write.table(FE_test, "data/fe_test_1.csv", quote=FALSE, sep=",", row.names=FALSE)



###### Removing tables() ######
rm(list=tables()$NAME)
gc()
