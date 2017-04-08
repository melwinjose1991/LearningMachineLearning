library("jsonlite")
library("magrittr")
library("dplyr")
library("purrr")
library(h2o)
library(data.table)

data = fromJSON("../data/train.json")

features <- data$features
photos <- data$photos

data$features <- NULL
data$photos <- NULL 

vars = setdiff(names(data), c("photos", "features"))
data = map_at(data, vars, unlist) %>% tibble::as_tibble(.)

data$features <- features
data$photos <- photos

rm(features)
rm(photos)



## Helper functions
getAccuracy=function(predictions, actual){
  percent = mean(predictions == actual)
  print(paste('Accuracy',percent))
}


## Feature Engineering
#bathrooms
data$bathrooms = as.numeric(as.character(data$bathrooms))

# bedrooms
data$bedrooms = as.numeric(as.character(data$bedrooms))

# price
data$price = as.numeric(as.character(data$price))



## 
x = c("bathrooms", "bedrooms", "price")
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


## validation strategy
xd = h2o.splitFrame(h2o_train,ratios = 0.6)
split_val = xd[[2]]


## Training model
gbm_clf <- h2o.gbm(x = x
                   ,y = y
                   ,training_frame = h2o_train
                   ,validation_frame = split_val
                   ,ntrees = 200
                   ,min_rows = 150
                   ,stopping_rounds = 10
                   ,learn_rate = 0.05
                   ,sample_rate = 0.8
)

gbm_clf_pred = as.data.table(h2o.predict(gbm_clf, h2o_test))
predictions = gbm_clf_pred$predict
getAccuracy(predictions, as.factor(test$interest_level))



## Test Data
real_test = fromJSON("../data/test.json")

features <- real_test$features
photos <- real_test$photos

real_test$features <- NULL
real_test$photos <- NULL 

real_test = map_at(real_test, x, unlist) %>% tibble::as_tibble(.)
id = unname(sapply(real_test$listing_id, `[[`, 1))
real_test2 = real_test[, x]

#real_test$features <- features
#real_test$photos <- photos

rm(features)
rm(photos)

real_test_h2o = as.h2o(real_test2)
gbm_clf_pred = as.data.table(h2o.predict(gbm_clf, real_test_h2o))

to_write = data.frame("listing_id"=id, "high"=gbm_clf_pred$high, "medium"=gbm_clf_pred$medium, "low"=gbm_clf_pred$low)
write.csv(to_write, file="gbm_3v.csv", row.names = FALSE, quote = FALSE)

