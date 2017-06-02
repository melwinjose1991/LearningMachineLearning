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



# product_type
train$product_type_int = as.numeric(train$product_type)

test[is.na(test$product_type),"product_type"] = rep("Investment",sum(is.na(test$product_type)))
test$product_type_int = rep(1,dim(test)[1])
test[test$product_type=="OwnerOccupier",]$product_type_int = 2



# sub_area
## Do we need to add other sub-area specific
## properties ???
getMap=function(df1_col, df2_col){
  all_values = unique(c(unique(as.character(df1_col)),unique(as.character(df2_col))))
  map = list()
  index = 1
  print(all_values)
  for(value in all_values){
    map[value] = index
    index = index + 1
  }
  print(map)
  map
}

getFromMap=function(key, map){
  if(key %in% names(map)){
    map[[key]]
  }else{
    NA
  }
}

map = getMap(train$sub_area, test$sub_area)
train$sub_area_int = unlist(lapply(train$sub_area, function(x) getFromMap(as.character(x), map)))
test$sub_area_int = unlist(lapply(test$sub_area, function(x) getFromMap(as.character(x), map)))



# area_m, full_all, pop_density_m
train$pop_density_m = train$full_all/train$area_m
test$pop_density_m = test$full_all/test$area_m



# metro_km_
# _avto : No NAs
# _walk : NAs in both test and train
med_km_walk = median(c(train$metro_km_walk, test$metro_km_walk), na.rm = TRUE)
train[is.na(train$metro_km_walk),"metro_km_walk"] = med_km_walk
test[is.na(test$metro_km_walk),"metro_km_walk"] = med_km_walk



# 500mts
n = names(train)
all_500 = n[grepl("count_500$",n)]
# what about cafe_count_500_* ???
for(c in all_500){
  if( sum(is.na(train[,c]))>0 | sum(is.na(test[,c]))>0 ){
    print(c)
    med = median(c(train[,c],test[,c]), na.rm=TRUE)
    train[is.na(train[,c]),c] = med
    test[is.na(test[,c]),c] = med
  }
}



# km
n = names(train)
all_km = n[grepl("km$",n)]
for(km in all_km){
  if( sum(is.na(train[,km]))>0 | sum(is.na(test[,km]))>0 ){
    print(km)
    med = median(c(train[,km],test[,km]), na.rm=TRUE)
    train[is.na(train[,km]),km] = med
    test[is.na(test[,km]),km] = med
  }
}



names(train)[train[1,]!=train[8,]]



# variables
x = c(
  "year",
  "month",
  "day",
  
  "full_sq",
  "life_sq",
  "life_sq_fraction",
  
  "product_type_int",
  "sub_area_int",
  "area_m",
  "full_all",
  "pop_density_m",
  
  "metro_km_avto",
  "metro_km_walk",
  
  all_500,
  all_km
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
               min_child_weight    = 25,
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
                     eval_metric         = "rmse",
                     print_every_n = 25
)
#   3223106.250000 - 0.361
#   2786899.750000 - 0.345 
#   2690190.000000 - 0.337 50/73
 
imp = xgb.importance(feature_names = x, model = model)
imp



## Test Prediction
test_pred = predict(model, test_DM)
pred = data.frame("id"=test$id, "price_doc"=test_pred)

file_name = paste0("xgb_", "x", toString(length(x)), "_r",nrounds, ".csv")
write.csv(pred, file_name, row.names = FALSE)
