library(xgboost)

train = read.csv("data/train.csv", header=TRUE, sep=",")
test = read.csv("data/test.csv", header=TRUE, sep=",")



# timestamp
# NO NAs or ""
getTimePart=function(date_str, part=1){
  unlist(strsplit(date_str, split="-"))[1]
}

train$year = as.numeric(unlist(lapply(train$timestamp, FUN=function(x) getTimePart(as.character(x),1))))
train$month = as.numeric(unlist(lapply(train$timestamp, FUN=function(x) getTimePart(as.character(x),2))))
train$day = as.numeric(unlist(lapply(train$timestamp, FUN=function(x) getTimePart(as.character(x),3))))

test$year = as.numeric(unlist(lapply(test$timestamp, FUN=function(x) getTimePart(as.character(x),1))))
test$month = as.numeric(unlist(lapply(test$timestamp, FUN=function(x) getTimePart(as.character(x),2))))
test$day = as.numeric(unlist(lapply(test$timestamp, FUN=function(x) getTimePart(as.character(x),3))))



# full_sq
# NO NAs or 0 or ""



# life_sq
# NAs exists
median_life_sq = median(c(train$life_sq,test$life_sq), na.rm=TRUE)
train[is.na(train$life_sq),"life_sq"] = median_life_sq
test[is.na(test$life_sq),"life_sq"] = median_life_sq


# life_sq_faction
train$life_sq_fraction = train$life_sq/train$full_sq
test$life_sq_fraction = test$life_sq/test$full_sq



# variables
x = c(
  "year",
  "month",
  "day",
  
  "full_sq",
  "life_sq",
  "life_sq_fraction"
)

y = c("price_doc")



# Training
rows = dim(train)[1]
train_rows = sample(1:rows, 0.80*rows, replace=F)

train_DM <- xgb.DMatrix(data = as.matrix(train[train_rows,x]), label=train[train_rows,y])
valid_DM <- xgb.DMatrix(data = as.matrix(train[-train_rows,x]), label=train[-train_rows,y])
test_DM <- xgb.DMatrix(data = as.matrix(test[,x]))

param = list(  objective           = "reg:linear", 
               booster             = "gbtree",
               eta                 = 0.0125,
               max_depth           = as.integer(length(x)),
               min_child_weight    = 50,
               subsample           = 0.8,
               colsample_bytree    = 0.60
)

nrounds = 800
model = xgb.train(   params              = param, 
                     data                = train_DM,
                     nrounds             = nrounds, 
                     early_stopping_rounds  = 20,
                     watchlist           = list(val=valid_DM),
                     maximize            = FALSE,
                     eval_metric         = "mae",
                     print_every_n = 25
)
 
imp = xgb.importance(feature_names = x, model = model)
imp



## Test Prediction
test_pred = predict(model, test_DM)
pred = data.frame("id"=test$id, "price_doc"=test_pred)

file_name = paste0("xgb_", "x", toString(length(x)), "_r",nrounds, ".csv")
write.csv(pred, file_name, row.names = FALSE)
