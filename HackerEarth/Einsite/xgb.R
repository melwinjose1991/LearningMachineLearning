library(xgboost)

train = read.csv("data/train.csv", header=TRUE, sep=",")
test = read.csv("data/test.csv", header=TRUE, sep=",")



## New.User
#   Some of them are "" in both train and test
train[train$new_user=="","new_user"] = "NO"
train$new_user_int = as.numeric(unlist(train[,"new_user"]))

test[test$new_user=="","new_user"] = "NO"
test$new_user_int = as.numeric(unlist(test[,"new_user"]))
# YES = 3, NO=2



## toll_price
# Outliers ?



## tip_amount
# Outliers ?
# NA ? 0?
train[is.na(train$tip_amount),"tip_amount"] = 0

test[is.na(test$tip_amount),"tip_amount"] = 0



x = c("new_user_int", "tolls_amount", "tip_amount")
y = c("fare_amount")

train_DM <- xgb.DMatrix(data = as.matrix(train[,x]), label=train[,y])
test_DM <- xgb.DMatrix(data = as.matrix(test[,x]))

param = list(  objective           = "reg:linear", 
                booster             = "gbtree",
                eta                 = 0.025,
                max_depth           = as.integer(length(x)),
                min_child_weight    = 100,
                subsample           = 0.8
                #colsample_bytree    = 0.50
)

nrounds = 800
model = xgb.train(   params              = param, 
                     data                = train_DM,
                     nrounds             = nrounds, 
                     early_stopping_rounds  = 20,
                     watchlist           = list(val=train_DM),
                     maximize            = FALSE,
                     eval_metric         = "mae",
                     print_every_n = 25
)

test_pred = predict(model, test_DM)
pred = data.frame("TID"=test$TID, "fare_amount"=test_pred)

file_name = paste0("xgb_", "x", toString(length(x)), "_r",nrounds, ".csv")
write.csv(pred, file_name, row.names = FALSE)
