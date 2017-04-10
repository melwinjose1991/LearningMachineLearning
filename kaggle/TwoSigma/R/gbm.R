library(jsonlite)
library(magrittr)
library(dplyr)
library(purrr)
library(h2o)
library(data.table)
library(stringr)

data = fromJSON("../data/train.json")
real_test = fromJSON("../data/test.json")


data_features <- data$features
data_photos <- data$photos
data$features <- NULL
data$photos <- NULL 

test_features <- real_test$features
test_photos <- real_test$photos
real_test$features <- NULL
real_test$photos <- NULL 


vars = setdiff(names(data), c("photos", "features"))
data = map_at(data, vars, unlist) %>% tibble::as_tibble(.)
real_test = map_at(real_test, vars, unlist) %>% tibble::as_tibble(.)


data$features <- data_features
data$photos <- data_photos

real_test$features = test_features
real_test$photos = test_photos

rm(data_features)
rm(data_photos)
rm(test_features)
rm(test_photos)



## Helper functions
getAccuracy=function(predictions, actual){
  percent = mean(predictions == actual)
  print(paste('Accuracy',percent))
}

getPivotTableAvg = function(variable, target){
  pivot_1 = aggregate(target~variable, data, FUN=sum)
  pivot_2 = aggregate(target~variable, data, FUN=length)
  pivot_2["target"] = pivot_1["target"]/pivot_2["target"]
  pivot_2
}

getWordCount=function(target, N){
  df = as.data.frame(sort(table(tolower(unlist(data[data$interest_level==target,"features"],' '))), 
                          decreasing = TRUE)[1:N])
  df$total = lapply(df$Var1, FUN=function(x) length(grep(x,tolower(data$features))) )
  df$total = as.numeric(df$total)
  df$per = df$Freq / df$total
  df
}

getRows=function(word){
  grep(word, tolower(data$features))
}

freq_features = c("elevator", "cats allowed", "dogs allowed", "doorman", "hardwood", "dishwasher", 
                  "laundry", "no fee", "fitness", "war", "deck", "dining", "outdoor", "internet",
                  "balcony", "swimming", "exclusive", "terrace", "loft", "wheelchair")
createFeatureCol=function(){
  for(f in freq_features){
    print(f)
    data[f] = rep(0, rows)
    data[getRows(f),f] = 1
    data[f] = factor(data[[f]])
  }
}



## Feature Engineering

#bathrooms
data$bathrooms = as.numeric(as.character(data$bathrooms))
data = data[!data$bathrooms>6,]

real_test$bathrooms = as.numeric(as.character(real_test$bathrooms))


# bedrooms
data$bedrooms = as.numeric(as.character(data$bedrooms))
data = data[!data$bedrooms>=7,]

real_test$bedrooms = as.numeric(as.character(real_test$bedrooms))

#dbath = as.data.frame(table(data$bedrooms, data$interest_level))
#rbath = reshape(dbath, timevar="Var2", v.names = "Freq", idvar="Var1", direction = "wide")
#barplot(t(rbath))


# total rooms
data$rooms = data$bathrooms + data$bedrooms
real_test$rooms = real_test$bathrooms + real_test$bedrooms


# bedrooms/bathrooms
data$bathbed = data$bathrooms / data$bedrooms
data[is.na(data$bathbed), c("bathbed")] = -1
data[is.infinite(data$bathbed), c("bathbed")] = 100
data$bathbed = floor(data$bathbed)

real_test$bathbed = real_test$bathrooms / real_test$bedrooms
real_test[is.na(real_test$bathbed), c("bathbed")] = -1
real_test[is.infinite(real_test$bathbed), c("bathbed")] = 100
real_test$bathbed = floor(real_test$bathbed)


# price
data$price = as.numeric(as.character(data$price))
real_test$price = as.numeric(as.character(real_test$price))


# price/bedrooms
data$bed_price = data$price / data$bedrooms
data[which(is.infinite(data$bed_price)),"bed_price"] = data[which(is.infinite(data$bed_price)),"price"]

real_test$bed_price = real_test$price / real_test$bedrooms
real_test[which(is.infinite(real_test$bed_price)),"bed_price"] = real_test[which(is.infinite(real_test$bed_price)),"price"]


# created
getMonth=function(x){
  as.POSIXlt(x[[1]], format="%Y-%m-%d%t%H:%M:%S")$mon
}
getDay=function(x){
  as.POSIXlt(x[[1]], format="%Y-%m-%d%t%H:%M:%S")$mday
}
getHour=function(x){
  as.POSIXlt(x[[1]], format="%Y-%m-%d%t%H:%M:%S")$hour
}
getMin=function(x){
  as.POSIXlt(x[[1]], format="%Y-%m-%d%t%H:%M:%S")$min
}
getSec=function(x){
  as.POSIXlt(x[[1]], format="%Y-%m-%d%t%H:%M:%S")$sec
}

if(FALSE){
  data$month = unlist(lapply(data$created, FUN=getMonth))
  data$day = unlist(lapply(data$created, FUN=getDay))
  data$hour = unlist(lapply(data$created, FUN=getHour))
  data$min = unlist(lapply(data$created, FUN=getMin))
  data$sec = unlist(lapply(data$created, FUN=getSec))
  
  real_test$month = unlist(lapply(real_test$created, FUN=getMonth))
  real_test$day = unlist(lapply(real_test$created, FUN=getDay))
  real_test$hour = unlist(lapply(real_test$created, FUN=getHour))
  real_test$min = unlist(lapply(real_test$created, FUN=getMin))
  real_test$sec = unlist(lapply(real_test$created, FUN=getSec))
}


# feature length
data$f_len = lengths(data$features)
real_test$f_len = lengths(real_test$features)


# num of pictures
data$nphotos = lengths(data$photos)
real_test$nphotos = lengths(real_test$photos)


# manager_id
data$manager_id = factor(data$manager_id)
real_test$manager_id = factor(real_test$manager_id)


# specific features
rows=dim(data)[1]
for(f in freq_features){
  print(f)
  data[f] = rep(0, rows)
  data[getRows(f),f] = 1
  data[f] = factor(data[[f]])
}

rows_test=dim(real_test)[1]
for(f in freq_features){
  print(f)
  real_test[f] = rep(0, rows_test)
  real_test[getRows(f),f] = 1
  real_test[f] = factor(real_test[[f]])
}


# building_id
isBuildingLvl=function(bldg_id){
  lvls = data[data$building_id==bldg_id,"interest_level"]
  total = dim(lvls)[1]
  low_lvls = sum(lvls$interest_level=="low") / total
  med_lvls = sum(lvls$interest_level=="medium") / total 
  high_lvls = sum(lvls$interest_level=="hgih") / total
  pers = c(low_lvls, med_lvls, high_lvls)
  pers
}

if(FALSE){
  data["high_bldg"] = 0
  data["med_bldg"] = 0
  data["low_bldg"] = 0
  high_map= c()
  med_map= c()
  low_map= c()
  
  unique_ids = unique(data$building_id)
  for(b_id in unique_ids){
    distb = isBuildingLvl(b_id)
    if(distb[1] == max(distb)){
      low_map = c(low_map, b_id)
      data[data$building_id==b_id,"low_bldg"] = 1
    }
    if(distb[2] == max(distb)){
      med_map = c(med_map, b_id)
      data[data$building_id==b_id,"med_bldg"] = 1
    }
    if(distb[3] == max(distb)){
      high_map = c(high_map, b_id)
      data[data$building_id==b_id,"high_bldg"] = 1
    }
  }
  
  real_test["high_bldg"] = 0
  real_test["med_bldg"] = 0
  real_test["low_bldg"] = 0
  real_test[real_test$building_id %in% low_map,"low_bldg"] = 1
  real_test[real_test$building_id %in% med_map,"med_bldg"] = 1
  real_test[real_test$building_id %in% high_map,"high_bldg"] = 1
}

data$building_id = factor(data$building_id)
real_test$building_id = factor(real_test$building_id)


## 
x = c("bathrooms", "bedrooms", "bathbed", "price", 
      "f_len", "manager_id", "rooms", 
      "nphotos", "bed_price", freq_features, 
      #"building_id"
      "low_bldg", "med_bldg", "high_bldg"
      )
y = c("interest_level")
x_y = c(x,y)
rows = dim(data)[1]
train_rows = sample(1:rows, 0.75*rows, replace=F)
train = data[, x_y]
test = data[-train_rows, x_y]


### h2o initialization ###
h2o.init(nthreads = -1, max_mem_size = "4G") 
h2o_train = as.h2o(train)
h2o_test = as.h2o(test)
h2o_train$interest_level = as.factor(h2o_train$interest_level)
h2o_test$interest_level = as.factor(h2o_test$interest_level)


## Training model
gbm_clf <- h2o.gbm(x = x
                   ,y = y
                   ,training_frame = h2o_train
                   ,distribution = "multinomial"
                   ,stopping_metric = "logloss"
                   ,ntrees = 600
                   ,max_depth = length(x)
                   ,min_rows = 200
                   ,stopping_rounds = 10
                   ,learn_rate = 0.025
                   ,sample_rate = 0.8
                   ,col_sample_rate = 0.8
                   ,model_id = "gbm_31"
)

gbm_clf_pred = as.data.table(h2o.predict(gbm_clf, h2o_test))
predictions = gbm_clf_pred$predict
getAccuracy(predictions, as.factor(test$interest_level))


## Train only on train_rows
# building vector 600   200   0.781   0.??  xx  no building_id,  
# building_id     600   200   0.738   0.62  ok  we were over-fitting !!!
# building_id     650   150   0.737   0.64  ok  we were over-fitting !!!      
# building_id     700   100   0.740   0.68  xx  we are over-fitting, try 150=min_rows
# m/d/h/m/s       700         0.786   0.97  xx    
# m/d/h/m/s       400         0.786         xx
# building vector 400         0.77    0.79  xx  no building_id,   
# building_id     400         0.736   0.65  ok  no building encoding



### >>> TRAIN ON ALL THE TRAINING DATA and then proceed <<< ### 
## Test Data
id = unname(sapply(real_test$listing_id, `[[`, 1))

real_test2 = real_test[, x]

real_test_h2o = as.h2o(real_test2)
gbm_clf_pred = as.data.table(h2o.predict(gbm_clf, real_test_h2o))

to_write = data.frame("listing_id"=id, "high"=gbm_clf_pred$high, "medium"=gbm_clf_pred$medium, "low"=gbm_clf_pred$low)
write.csv(to_write, file="gbm_xv.csv", row.names = FALSE, quote = FALSE)

