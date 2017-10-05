library(xgboost)
library(data.table)



###### Removing tables() ######
rm(list=tables()$NAME)
gc()



###### reading the csv files ######
train = fread("data/fe_train_1.csv")
test = fread("data/fe_test_1.csv")



###### X & Y ######
x = colnames(test)
x = x[!x %in% c("id")]


if(FALSE){
  for(col in x){
    train[,col] = as.numeric(train[[col]])
    test[,col] = as.numeric(test[[col]])
  }
}


###### train/test split ######
rows = dim(train)[1]
train_rows = sample(1:rows, 0.80*rows, replace=F)

train_DM <- xgb.DMatrix(data = as.matrix(train[train_rows,x,with=FALSE]), 
                        label=train[train_rows, target])
valid_DM <- xgb.DMatrix(data = as.matrix(train[-train_rows,x,with=FALSE]), 
                        label=train[-train_rows, target])
rm(train)
gc()
tables()



###### Training ######
seed_used = 1234
param = list(  
  objective           = "binary:logistic", 
  booster             = "gbtree",
  max_depth           = as.integer(length(x)/10),
  eta                 = 0.0125,
  gamma               = 0,
  colsample_bytree    = 0.8,
  min_child_weight    = 50,
  subsample           = 1,
  seed                = seed_used
)
nrounds = 500

model = xgb.train(   params              = param, 
                     data                = train_DM,
                     nrounds             = nrounds, 
                     early_stopping_rounds  = 20,
                     watchlist           = list(val=valid_DM),
                     maximize            = FALSE,
                     eval_metric         = "auc",
                     print_every_n = 25
)

imp = as.data.frame(xgb.importance(feature_names = x, model = model))
imp

# 0.584 - 0.172


###### Test ######
test_DM = xgb.DMatrix(data = as.matrix(test[, x, with=FALSE]))
test_pred = predict(model, test_DM)

pred = data.frame(id=test$id, target=test_pred)
dim(pred)[1] == dim(test)[1]
rm(test)
gc()

write.csv(pred, "submission_xgb.csv", row.names = FALSE, quote=FALSE)

