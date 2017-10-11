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
train = fread("data/train.csv")
test = fread("data/test.csv")

train_pca = fread("data/afe_train_pca_1.csv")
train_pca[,id:=NULL]
train_pca[,target:=NULL]
test_pca = fread("data/afe_test_pca_1.csv")
test_pca[,id:=NULL]

train = cbind(train, train_pca)
test = cbind(test, test_pca)
dim(train)
dim(test)

rm(train_pca)
rm(test_pca)



###### X & Y ######
x = colnames(test)
x = x[!x %in% c("id")]
x


for(col in x){
  train[,col] = as.numeric(train[[col]])
  test[,col] = as.numeric(test[[col]])
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
  max_depth           = as.integer(length(x)/2),
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
                     feval                  = xgb_normalizedgini,
                     maximize               = TRUE,
                     print_every_n          = 10
)

imp = as.data.frame(xgb.importance(feature_names = x, model = model))
imp

# 0.218 - 0.204
# 0.252 - 0.252    - just using the basic features
# 0.273 - 0.252    - basic + PCA



###### Test ######
test_DM = xgb.DMatrix(data = as.matrix(test[, x, with=FALSE]))
test_pred = predict(model, test_DM)

pred = data.frame(id=test$id, target=test_pred)
dim(pred)[1] == dim(test)[1]
rm(test)
gc()

write.csv(pred, "submission_xgb.csv", row.names = FALSE, quote=FALSE)

