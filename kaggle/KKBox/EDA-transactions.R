### Exploratory Data Analysis

library(data.table)



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
