library(xgboost)
library(data.table)



###### Removing tables() ######
rm(list=tables()$NAME)
gc()



###### reading the csv files ######
train_trans = fread("data/fe_train_transactions.csv")
train_members = fread("data/fe_train_members.csv")
train = merge(train_trans, train_members, all.x=TRUE)
head(train)

test_trans = fread("data/fe_test_transactions.csv")
test_members = fread("data/fe_test_members.csv")
test = merge(test_trans, test_members, all.x=TRUE)
head(test)

rm(train_trans)
rm(train_members)
rm(test_trans)
rm(test_members)



###### X & Y ######
x = c("nof_transcations",
      "mode_pay_id", "payment_types",
      "mode_plandays", "plandays_unique",
      "mode_pay_code", "paid_less", "paid_equal", "paid_more",
      "mode_auto_renew", "percent_auto_renews",
      "historical_churns",
      
      "city", "bd")

for(col in x){
  train[,col] = as.numeric(train[[col]])
  test[,col] = as.numeric(test[[col]])
}



###### train/test split ######
rows = dim(train)[1]
train_rows = sample(1:rows, 0.80*rows, replace=F)

train_DM <- xgb.DMatrix(data = as.matrix(train[train_rows,x,with=FALSE]), 
                        label=train[train_rows, is_churn])
valid_DM <- xgb.DMatrix(data = as.matrix(train[-train_rows,x,with=FALSE]), 
                        label=train[-train_rows, is_churn])


###### Training ######
seed_used = 1234
param = list(  
  objective           = "binary:logistic", 
  booster             = "gbtree",
  max_depth           = as.integer(length(x)/2),
  eta                 = 0.0125,
  gamma               = 0,
  colsample_bytree    = 1,
  min_child_weight    = 50,
  subsample           = 1,
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

# 0.1933  - 0.2457 
# 0.1808  - 0.2407  d = 2
# 0.1785  - 0.2406  d = 2 Tuned
# 0.1748  - 0.2372
# 0.1725  - 0.236

# for a change of 0.01 there is a change of 0.005



###### Test ######
test_DM = xgb.DMatrix(data = as.matrix(test[, x, with=FALSE]))

test_pred = predict(model, test_DM)

pred = data.frame(msno=test$msno, is_churn=test_pred)
dim(pred)[1] == dim(test)[1]

write.csv(pred, "submission_xgb.csv", row.names = FALSE, quote=FALSE)

