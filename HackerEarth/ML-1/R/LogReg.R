data = read.csv("../data/train_indessa.csv")
rows = dim(data)[1]

drops = c("desc")
data = data[, !(names(data) %in% drops)]

head(data, 10)

apply(data, 2, function(x) any(is.na(x)))

## Train and Test
train_rows = sample(1:rows, 0.75*rows, replace=F)
train = data[train_rows, ]
test = data[-train_rows, ]


## Helper functions
predictAccuracy=function(model, test_data, predictors_list){
  predictions = predict(model, newdata=subset(test_data,select=predictors_list), type="response")
  predictions = ifelse(predictions > 0.5,1,0)
  error_percent = mean(predictions != test_data$loan_status)
  print(paste('Accuracy',1-error_percent))
}

makePredictions=function(model, test_data, predictors_list){
  predictions = predict(model, newdata=subset(test_data,select=predictors_list), type="response")
  predictions
}


## LogRegression base-model
predictors = c("loan_amnt","funded_amnt","funded_amnt_inv")
basic_model = glm(loan_status~loan_amnt+funded_amnt+funded_amnt_inv, family=binomial(link='logit'), data=train)
predictAccuracy(basic_model, test, predictors)
## 0.7646
makePredictions(basic_model, test, predictors)


## Real-test data
real_test = read.csv("../data/test_indessa.csv")

test_predictors = c("loan_amnt","funded_amnt","funded_amnt_inv")
test_model = basic_model

test_predictions = makePredictions(test_model, real_test, test_predictors)

to_write = data.frame("member_id"=real_test$member_id, "loan_status"=test_predictions)
write.csv(to_write, file="LogReg.csv", row.names = FALSE, quote = FALSE)