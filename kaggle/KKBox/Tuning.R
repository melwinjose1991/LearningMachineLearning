library(xgboost)
library(data.table)
library(caret)



###### Removing tables() ######
rm(list=tables()$NAME)
gc()



###### reading the csv files ######
train = fread("data/fe_train_1.csv")
test = fread("data/fe_test_1.csv")



###### X & Y ######
x = c("mode_pay_id", "payment_types",
      "mode_plandays", "plandays_unique",
      "mode_pay_code", "paid_less", "paid_equal", "paid_more")

for(col in x){
  train[,col] = as.numeric(train[[col]])
  test[,col] = as.numeric(test[[col]])
}



###### train/test split ######
rows = dim(train)[1]
train_rows = sample(1:rows, 0.50*rows, replace=F)

train_DM <- xgb.DMatrix(data = as.matrix(train[train_rows,x,with=FALSE]), 
                        label=train[train_rows, is_churn])
valid_DM <- xgb.DMatrix(data = as.matrix(train[-train_rows,x,with=FALSE]), 
                        label=train[-train_rows, is_churn])



###### Tuning ######
cv.ctrl = trainControl(
  method = "repeatedcv", 
  repeats = 1,
  number = 2, 
  #classProbs = TRUE,
  allowParallel=T,
  verboseIter = TRUE
)

xgb.grid = expand.grid(
  nrounds = 1000,
  max_depth = 1:8,
  eta = c(0.025),
  gamma = 0,
  colsample_bytree    = c(0.8, 0.9, 1),
  min_child_weight    = c(50, 100, 200),
  subsample           = c(0.8, 0.9, 1)
)

xgb_tune = train(
  x = as.matrix(train[train_rows, x, with=FALSE]),
  y = as.factor(train[train_rows, is_churn]),
  method = "xgbTree",
  trControl = cv.ctrl,
  tuneGrid = xgb.grid,
  verbose = T,
  metric = "logloss",
  nthread = 4
)
  
  

###### Test ######
test_DM = xgb.DMatrix(data = as.matrix(test[, x, with=FALSE]))

test_pred = predict(model, test_DM)

pred = data.frame(msno=test$msno, is_churn=test_pred)
dim(pred)[1] == dim(test)[1]

write.csv(pred, "submission_xgb.csv", row.names = FALSE, quote=FALSE)

