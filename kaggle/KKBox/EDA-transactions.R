### Exploratory Data Analysis

library(data.table)
library(ggplot2)


###### Removing tables() ######
rm(list=tables()$NAME)
gc()



###### reading the csv files ######
eda_train_data = fread("data/train.csv")
eda_transactions_data = fread("data/transactions.csv")

head(eda_train_data)
head(eda_transactions_data)

str(eda_train_data)
str(eda_transactions_data)


setkey(eda_train_data, msno)
setkey(eda_transactions_data, msno)



###### Utility Functions ######
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


###### %Churns in train data ######
sum(eda_train_data$is_churn==1) / dim(eda_train_data)[1]
# 6.39% is churn



###### %Churns in train data ######
# logloss for all zero preidction = 3.111
# N = 970960

# p=0 is taken as max( min(0,1-1E-15), 1E-15) = 1E-15
# log(p=0) = -34.53878
# n_1 * -34.53878

# p=1 is taken as max( min(1,1-1E-15), 1E-15) = 1
# log(p=1) ~ 0
# n_0 * 0

# n_1 = (N*logloss)/34.538
# n_1 = 87459
# n_0 = 883501
# #test records = 970960

# % of churns in test = 87459/970960 = 9%



###### payment_method_id ######
merged_dt = merge(eda_train_data, eda_transactions_data[, .(msno, payment_method_id)], all.x=TRUE)

dt_payment_churn = merged_dt[, .(mode_pay_id=Mode(payment_method_id), 
                                 payment_types=length(unique(payment_method_id)),
                                 is_churn=max(is_churn)), 
                             by=msno]
head(dt_payment_churn)
dt_payment_churn[, msno:=NULL]

dt_payment_churn[, .(count=.N, churns=sum(is_churn), percent_churns=sum(is_churn)/.N), 
                                    by=mode_pay_id][order(-count)]

dt_payment_churn[, .(count=.N, churns=sum(is_churn), percent_churns=sum(is_churn)/.N),
                 by=payment_types]



###### payment_plan_days ######
merged_dt = merge(eda_train_data, eda_transactions_data[, .(msno, payment_plan_days)], all.x=TRUE)

dt_plandays_churn = merged_dt[, .(mode_plandays=Mode(payment_plan_days), is_churn=max(is_churn)), by=msno]
head(dt_plandays_churn)
dt_plandays_churn[, msno:=NULL]

dt_plandays_churn[, .(count=.N, churns=sum(is_churn), percent_churns=sum(is_churn)/.N), 
                 by=mode_plandays][order(-count)]



###### plan_list_price & actual_amount_paid ######
merged_dt = merge(eda_train_data, 
                  eda_transactions_data[, .(msno, plan_list_price, actual_amount_paid)], 
                  all.x=TRUE)

getCode = function(plan_list_price, actual_amount_paid){
  if(plan_list_price > actual_amount_paid){
    -1
  }else if(plan_list_price == actual_amount_paid){
    0
  }else{
    1
  }
}

merged_dt[, pay_code := mapply(getCode, plan_list_price, actual_amount_paid)]

dt_paycode_churn = merged_dt[, .(mode_pay_code=Mode(pay_code), 
                                 is_churn=max(is_churn),
                                 paid_less=sum(actual_amount_paid<plan_list_price),
                                 paid_equal=sum(actual_amount_paid==plan_list_price),
                                 paid_more=sum(actual_amount_paid>plan_list_price)), 
                             by=msno]
dt_paycode_churn[, msno:=NULL]

dt_paycode_churn[, .(count=.N, churns=sum(is_churn), percent_churns=sum(is_churn)/.N), 
                  by=mode_pay_code][order(-count)]

dt_paycode_churn[, .(count=.N, churns=sum(is_churn), percent_churns=sum(is_churn)/.N), 
                 by=paid_less][order(-count)]
dt_paycode_churn[, .(count=.N, churns=sum(is_churn), percent_churns=sum(is_churn)/.N), 
                 by=paid_equal][order(-count)]
dt_paycode_churn[, .(count=.N, churns=sum(is_churn), percent_churns=sum(is_churn)/.N), 
                 by=paid_more][order(-count)]



###### is_autorenew & transcations ######
merged_dt = merge(eda_train_data, 
                  eda_transactions_data[, .(msno, is_auto_renew)], 
                  all.x=TRUE)

dt_renew_churn = merged_dt[, .( mode_auto_renew=Mode(is_auto_renew), 
                                auto_renew_ones = sum(is_auto_renew==1),
                                is_churn=max(is_churn),
                                nof_transcations = .N
                               ), 
                             by=msno]
dt_renew_churn[, percent_renew := (100*auto_renew_ones)/nof_transcations]
head(dt_renew_churn[percent_renew!=100])

breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110)
# 0 : [0,10)
# 1 : [10,20) and so on
dt_renew_churn[, percent_renew_bin:= .bincode(percent_renew, breaks, FALSE, TRUE) - 1 ]
# head(dt_renew_churn)
# head(dt_renew_churn[percent_renew>=90 & percent_renew<100], 40)
# head(dt_renew_churn[percent_renew==100])

d = dt_renew_churn[, .(count=.N, churns=sum(is_churn), percent_churns=sum(is_churn)/.N), 
                 by=percent_renew_bin][order(-count)]
d

ggplot(data=d, aes(x=percent_renew_bin, y=percent_churns)) + geom_bar(stat="identity")
