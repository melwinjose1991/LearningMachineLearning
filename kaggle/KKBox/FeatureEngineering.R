library(data.table)



###### reading the csv files ######
train_data = fread("data/train.csv")
test_data = fread("data/sample_submission_zero.csv")
transactions_data = fread("data/transactions.csv")



###### Utility Functions ######
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



###### mode_payment_id, payment_types ######
merged = merge(train_data, transactions_data[, .(msno, payment_method_id)], all.x=TRUE)
dt_train = merged[, .(mode_pay_id=Mode(payment_method_id), 
                      payment_types=length(unique(payment_method_id)),
                      is_churn=max(is_churn)), 
                  by=msno]
head(dt_train)
dim(dt_train)[1] == dim(train_data)[1]

merged = merge(test_data, transactions_data[, .(msno, payment_method_id)], all.x=TRUE)
dt_test = merged[, .(mode_pay_id=Mode(payment_method_id), 
                     payment_types=length(unique(payment_method_id)),
                     is_churn=max(is_churn)), 
                 by=msno]
head(dt_test)
dim(dt_test)[1] == dim(test_data)[1]



###### Write train and test ######
write.table(dt_train, "data/fe_train_1.csv", quote=FALSE, sep=",", row.names=FALSE)
write.table(dt_test, "data/fe_test_1.csv", quote=FALSE, sep=",", row.names=FALSE)



