library(xgboost)


###### reading the csv files ######
train = fread("data/fe_train_1.csv")
test = fread("data/fe_test_1.csv")



###### X & Y ######
train[, mode_pay_id:=as.numeric(mode_pay_id)]
test[, mode_pay_id:=as.numeric(mode_pay_id)]

train[, payment_types:=as.numeric(payment_types)]
test[, payment_types:=as.numeric(payment_types)]

x = c("mode_pay_id", "payment_types")



###### train/test split ######
rows = dim(train)[1]
train_rows = sample(1:rows, 0.80*rows, replace=F)

train_DM <- xgb.DMatrix(data = as.matrix(train[train_rows,x,with=FALSE]), 
                        label=train[train_rows, is_churn])
valid_DM <- xgb.DMatrix(data = as.matrix(train[-train_rows,x,with=FALSE]), 
                        label=train[-train_rows, is_churn])


###### Training ######
seed_used = 1234
param = list(  objective           = "binary:logistic", 
               booster             = "gbtree",
               eta                 = 0.0125,
               max_depth           = as.integer(length(x)/1),
               min_child_weight    = 75,
               subsample           = 1,
               colsample_bytree    = 0.70,
               seed                = seed_used
)

nrounds = 2000

model = xgb.train(   params              = param, 
                     data                = train_DM,
                     nrounds             = nrounds, 
                     early_stopping_rounds  = 20,
                     watchlist           = list(val=valid_DM),
                     maximize            = FALSE,
                     eval_metric         = "logloss",
                     print_every_n = 25
)

imp = as.data.frame(xgb.importance(feature_names = x, model = model))
imp



###### Test ######
test_DM = xgb.DMatrix(data = as.matrix(test[, x, with=FALSE]))

test_pred = predict(model, test_DM)

pred = data.frame(msno=test$msno, is_churn=test_pred)
dim(pred)[1] == dim(test)[1]

write.csv(pred, "submission_xgb.csv", row.names = FALSE, quote=FALSE)

