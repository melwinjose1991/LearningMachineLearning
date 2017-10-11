library(xgboost)
library(data.table)
library(MLmetrics)

xgb_normalizedgini <- function(preds, dtrain){
  actual <- getinfo(dtrain, "label")
  score <- NormalizedGini(preds,actual)
  return(list(metric = "NormalizedGini", value = score))
}

###### Removing tables() ######
rm(list=tables()$NAME)
gc()



###### reading the csv files ######
train = fread("data/afe_train_cat_1.csv")
test = fread("data/afe_test_cat_1.csv")
dim(train)
dim(test)


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
train_rows = sample(1:rows, 0.90*rows, replace=F)

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

model = xgb.train(   params                 = param, 
                     data                   = train_DM,
                     nrounds                = nrounds, 
                     early_stopping_rounds  = 20,
                     watchlist              = list(val=valid_DM),
                     #eval_metric           = "auc",
                     feval                  = xgb_normalizedgini,
                     maximize               = TRUE,
                     print_every_n          = 10
)

imp = as.data.frame(xgb.importance(feature_names = x, model = model))
imp

# 0.218 - 0.204


###### Test ######
test_DM = xgb.DMatrix(data = as.matrix(test[, x, with=FALSE]))
test_pred = predict(model, test_DM)

pred = data.frame(id=test$id, target=test_pred)
dim(pred)[1] == dim(test)[1]
rm(test)
gc()

write.csv(pred, "submission_xgb.csv", row.names = FALSE, quote=FALSE)

