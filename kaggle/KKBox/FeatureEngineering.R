library(data.table)



###### Removing tables() ######
rm(list=tables()$NAME)
gc()



###### reading the csv files ######
read_csv = "members"

train_data = fread("data/train.csv")
test_data = fread("data/sample_submission_zero.csv")

if(read_csv == "transactions"){
  transactions_data = fread("data/transactions.csv")
}else if(read_csv == "members"){
  members_data = fread("data/members.csv")
}



###### Utility Functions ######
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



###### FE on transactions.csv ######

if(read_csv == "transactions"){

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



###### historical_churns ######
no_of_records_processed = 0
total_records = dim(train_data)[1]

getHistoricalChurns = function(trans_dates, deadline_dates, cancels, msno="-"){
  
  no_of_records_processed <<- no_of_records_processed + 1
  if(no_of_records_processed %% 1000 == 0){
    print(paste0("Reached #",no_of_records_processed," / ", total_records))
  }
  
  if( length(trans_dates)==0 || length(deadline_dates)==0 || length(cancels)==0 || 
      is.na(trans_dates)  || is.na(deadline_dates)  || is.na(cancels) ){
    print(paste0(msno, ", has zero transactions."))
    return(0)
  }
  
  dt = data.table(x=trans_dates, y=deadline_dates, z=cancels)
  dt = dt[order(x)]
  trans_dates = dt$x
  deadline_dates = dt$y
  cancels = dt$z
  
  churns = 0
  last_deadline = -1
  
  for(i in 1:length(deadline_dates)){
    if(cancels[i]==1){
      last_deadline = as.Date(as.character(deadline_dates[i]), "%Y%m%d")
      next
    }
    if(last_deadline==-1){
      last_deadline = as.Date(as.character(deadline_dates[i]), "%Y%m%d")
      next
    }
    
    trans_date = as.Date(as.character(trans_dates[i]), "%Y%m%d")
    
    paid_after = as.numeric(trans_date - last_deadline)
    if(paid_after>30){
      #print(paste0(trans_date, " ", last_deadline, " diff:", paid_after))
      churns = churns + 1
    }
    
    last_deadline = as.Date(as.character(deadline_dates[i]), "%Y%m%d")
    
  }
  return(churns)
}

no_of_records_processed = 0
total_records = dim(train_data)[1]
merged = merge(train_data, 
               transactions_data[, .(msno, transaction_date, membership_expire_date,is_cancel)], 
               all.x=TRUE)
tmp = merged[, 
              .(historical_churns = getHistoricalChurns(transaction_date, membership_expire_date,is_cancel) )
              ,by=msno]
FE_train = merge(FE_train, tmp, by="msno", all.x=TRUE)
head(FE_train)

no_of_records_processed = 0
total_records = dim(test_data)[1]
merged = merge(test_data, 
               transactions_data[, .(msno, transaction_date, membership_expire_date,is_cancel)], 
               all.x=TRUE)
tmp = merged[, 
              .( historical_churns = getHistoricalChurns(transaction_date, membership_expire_date,is_cancel,msno) )
              ,by=msno]
FE_test = merge(FE_test, tmp, by="msno", all.x=TRUE)
head(FE_test)

if(FALSE){
  id = "+++l/EXNMLTijfLBa8p2TUVVVp2aFGSuUI/h7mLmthw="
  x = transactions_data[msno==id,
                        .(transaction_date, membership_expire_date, is_cancel), ]
  
  x[order(transaction_date)]
  
  trans_dates = x$transaction_date
  deadline_dates = x$membership_expire_date
  cancels = x$is_cancel
  
  getHistoricalChurns(trans_dates, deadline_dates, cancels)
}

} 
###### Version_1 ######



###### members.csv ######
if(read_csv == "members"){

  FE_train = merge(train_data, members_data, all.x=TRUE)
  head(FE_train)
  
  FE_test = merge(test_data, members_data, all.x=TRUE)
  head(FE_test)
  
  ### city ###
  
  ### bd ###
  FE_train[ bd < 0, bd := -1 ]
  FE_train[ bd > 123, bd := -2 ]
  sort(unique(FE_train$bd))
  
  FE_test[ bd < 0, bd := -1 ]
  FE_test[ bd > 123, bd := -2 ]
  sort(unique(FE_test$bd))
  
}
###### Version_2 ######



###### Write train and test ######
head(FE_train)
file_name = paste0("data/fe_train_", read_csv, ".csv")
write.table(FE_train, file_name, quote=FALSE, sep=",", row.names=FALSE)

head(FE_test)
file_name = paste0("data/fe_test_", read_csv, ".csv")
write.table(FE_test, file_name, quote=FALSE, sep=",", row.names=FALSE)



###### Removing tables() ######
rm(list=tables()$NAME)
gc()
