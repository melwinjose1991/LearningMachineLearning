library(data.table)
library(jsonlite)
library(xgboost)



#### Train
train <- fromJSON("data/train_data.json")

train_data <- data.table(ID = unlist(names(train)))
train_data[, `:=` (genres = unlist(lapply(train, '[',1)),
                   titles = unlist(lapply(train, '[',2)),
                   cities = unlist(lapply(train, '[', 3)),
                   segment = unlist(lapply(train, '[',4)),
                   dow = unlist(lapply(train, '[',5)),
                   tod = unlist(lapply(train, '[', 6))
)]



#### Test 
test <- fromJSON("data/test_data.json")

test_data <- data.table(ID  = unlist(names(test)))
test_data[,`:=` (genres = unlist(lapply(test, '[',1)),
                 titles = unlist(lapply(test, '[',2)),
                 tod = unlist(lapply(test, '[', 3)),
                 cities = unlist(lapply(test, '[',4)),
                 dow = unlist(lapply(test, '[',5))
)]



## Helper Functions
getMap=function(df1_col,df2_col){
  #df1=train_data[1:10,]$cities
  
  map = list()
  all_cities_str = unlist(df1_col)
  all_cities_str = c(all_cities_str, unlist(df2_col))
  
  for(cities_str in all_cities_str){
    splits = unlist(strsplit(cities_str,","))
    for(split in splits){
      key_value = unlist(strsplit(split,":"))
      if(key_value[1] %in% names(map)){
        #print(paste0("OLD:",key_value[1]))
        old_value = map[[key_value[1]]]
        map[key_value[1]] = old_value+1
      }else{
        #print(paste0("NEW:",key_value[1]))
        map[key_value[1]] = 1
      }
    }
  }
  map
}

getValue=function(value, key_to_search){
  #key_to_search = "Drama"
  #value = train_data[101,]$genres
  
  return_value = 0 
  splits = unlist(strsplit(value,","))
  for(split in splits){
    key_value = unlist(strsplit(split,":"))
    if(key_value[1]==key_to_search){
      return_value = as.numeric(key_value[2])
      break
    }
  }
  return_value
}



## Genres
genres_map = getMap(train_data$genres, test_data$genres)

all_genres = names(genres_map)[!names(genres_map) %in% "NA"]
for(col in names(all_genres)){
  if(col == "NA"){
    print("NA found ... skipping")
  }else{
    print(paste0("Creating ", col))
    train_data[,col] = sapply(train_data$genres, FUN=function(row_value) getValue(row_value, col))
    test_data[,col] = sapply(test_data$genres, FUN=function(row_value) getValue(row_value, col))
  }
}



## Hour of the day
tod_map = getMap(train_data$tod, test_data$tod)
all_tods = vector('character')

for(col in names(tod_map)){
  col_name = paste0("tod_", col)
  print(paste0("Creating ",col_name))
  all_tods = c(all_tods, col_name)
  
  train_data[,col_name] = sapply(train_data$tod, FUN=function(row_value) getValue(row_value, col))
  test_data[,col_name] = sapply(test_data$tod, FUN=function(row_value) getValue(row_value, col))
}



## Day of week
dow_map = getMap(train_data$dow, test_data$dow)
all_dows = vector('character')

for(col in names(dow_map)){
  col_name = paste0("dow_", col)
  print(paste0("Creating ",col_name))
  all_dows = c(all_dows, col_name)
  
  train_data[,col_name] = sapply(train_data$dow, FUN=function(row_value) getValue(row_value, col))
  test_data[,col_name] = sapply(test_data$dow, FUN=function(row_value) getValue(row_value, col))
}



## X and Y
x = c(all_genres, all_tods, all_dows)
y = c("segment_int")

train_data[train_data$segment=="pos","segment_int"] = 1
train_data[train_data$segment=="neg","segment_int"] = 0



## Training
rows = dim(train_data)[1]
train_rows = sample(1:rows, 0.80*rows, replace=F)

train_df = as.data.frame.matrix(train_data)
test_df = as.data.frame.matrix(test_data)

train_DM <- xgb.DMatrix(data = as.matrix(train_df[train_rows,x]), label=train_df[train_rows,y])
valid_DM <- xgb.DMatrix(data = as.matrix(train_df[-train_rows,x]), label=train_df[-train_rows,y])


seed_used = 1234
param = list(  objective           = "binary:logistic", 
               booster             = "gbtree",
               eta                 = 0.0125,
               max_depth           = as.integer(length(x)),
               min_child_weight    = 25,
               subsample           = 0.80,
               colsample_bytree    = 0.60,
               seed                = seed_used
)

nrounds = 1000
model = xgb.train(   params              = param, 
                     data                = train_DM,
                     nrounds             = nrounds, 
                     early_stopping_rounds  = 20,
                     watchlist           = list(val=valid_DM),
                     maximize            = FALSE,
                     eval_metric         = "auc",
                     print_every_n = 25
)
# 0.778 - 0.505

#imp = xgb.importance(feature_names = x, model = model)
#imp


## Test Prediction
test_df = as.data.frame.matrix(train_data)
test_DM <- xgb.DMatrix(data = as.matrix(test_df[,x]))

test_pred = predict(model, test_DM)
pred = data.frame("ID"=test_data$ID, "segment"=test_pred)
#pred[pred$fare_amount<0,]$fare_amount = 0

file_name = paste0("xgb_", "x", toString(length(x)), "_s",seed_used, ".csv")
write.csv(pred, file_name, row.names = FALSE)
