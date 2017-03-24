data = read.csv("../data/train_indessa.csv")
rows = dim(data)[1]

drops = c("desc")
data = data[, !(names(data) %in% drops)]

## Columns with NA
apply(data, 2, function(x) any(is.na(x)))

## Feature Engineering
# delinq_2yrs ||REPLACING NA||
sum(is.na(data$delinq_2yrs))
data[is.na(data$delinq_2yrs),"delinq_2yrs"] = 0

# pub_rec ||REPLACING NA||
sum(is.na(data$pub_rec))
data[is.na(data$pub_rec),"pub_rec"] = 0

# home_ownership ||MERGING CATEGORY VARIABLES||
levels(data$home_ownership) = c(levels(data$home_ownership), "NONE&OTHER")
data$home_ownership[data$home_ownership=="OTHER"] = "NONE&OTHER"
data$home_ownership[data$home_ownership=="NONE"] = "NONE&OTHER"
data$home_ownership = factor(data$home_ownership)

getPivotTable = function(variable){
  pivot_1 = aggregate(loan_status~variable, data, FUN=sum)
  pivot_2 = aggregate(loan_status~variable, data, FUN=length)
  pivot_2["defaults"] = pivot_1["loan_status"]
  pivot_2["avg"] = pivot_2["defaults"]/pivot_2["loan_status"]
  pivot_2
}

getNAs=function(column){
  print(sum(is.na(column)))
}

getUniqueEntries=function(column){
  print(unique(column))
}

getQuartile=function(column){
  data$quartile = with(data,factor(findInterval(column,c(-Inf,quantile(column,probs=c(0.25,.5,.75)),Inf)),
            labels=c("Q1","Q2","Q3","Q4")))
}

if(FALSE){
  
  # loan_amnt +ve coef, 
    # seems like funded_amnt and funded_amnt_inv are confounding loan_amnt
    # loan_amnt alone has -ve coef
  getQuartile(data$loan_amnt)
  
  # funded_amnt added, has a negative correlation with loan_status
    # check and plot the pivot table of funded_amnt
  
  # funded_amnt_inv added
  
  # term not significant
  
  # batch_enrolled ??? 105 levels, 2 empty levels 
  
  # int_rate
  
  
  
  # pub_rec has negative coef in the model
  getPivotTable(data$pub_rec)
  
  # adding grade woun't effect the accuracy
  # as each grade has equal avg of defaulters
  pivot_1 = aggregate(loan_status~grade, data, FUN=sum)
  pivot_2 = aggregate(loan_status~grade, data, FUN=length)
  pivot_2["defaults"] = pivot_1["loan_status"]
  pivot_2["avg"] = pivot_2["defaults"]/pivot_2["loan_status"]
  
  ## emp_length won;t effect the accuracy much
  
  ## home_ownership : combine NONE & OTHER
  ## ||OVER-FITTED|| !!! the count of NONE & OTHER are very less
  pivot_1 = aggregate(loan_status~home_ownership, data, FUN=sum)
  pivot_2 = aggregate(loan_status~home_ownership, data, FUN=length)
  pivot_2["defaults"] = pivot_1["loan_status"]
  pivot_2["avg"] = pivot_2["defaults"]/pivot_2["loan_status"]
  
}

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
basic_predictors = c("loan_amnt","funded_amnt","funded_amnt_inv")
basic_model = glm(loan_status~loan_amnt+funded_amnt+funded_amnt_inv, family=binomial(link='logit'), data=train)
predictAccuracy(basic_model, test, basic_predictors)
## 0.7646
makePredictions(basic_model, test, basic_predictors)

# better model
predictors_4 = c("loan_amnt","funded_amnt","funded_amnt_inv", "term", "emp_length", "home_ownership", "delinq_2yrs", "pub_rec")
model_4 = glm(loan_status~loan_amnt+funded_amnt+funded_amnt_inv+term+emp_length+home_ownership+delinq_2yrs+pub_rec, family=binomial(link='logit'), data=train)
predictAccuracy(model_4, test, predictors_4)
# 0.7667


predictors_5 = c("loan_amnt","pub_rec")
model_5 = glm(loan_status~loan_amnt+pub_rec, family=binomial(link='logit'), data=train)
summary(model_5)
predictAccuracy(model_5, test, predictors_5)



## Real-test data
real_test = read.csv("../data/test_indessa.csv")

# tranformations
real_test[is.na(real_test$delinq_2yrs),"delinq_2yrs"] = 0
real_test[is.na(real_test$pub_rec),"pub_rec"] = 0

levels(real_test$home_ownership) = c(levels(real_test$home_ownership), "NONE&OTHER")
real_test$home_ownership[real_test$home_ownership=="OTHER"] = "NONE&OTHER"
real_test$home_ownership[real_test$home_ownership=="NONE"] = "NONE&OTHER"
real_test$home_ownership = factor(real_test$home_ownership)



test_predictors = c("loan_amnt","funded_amnt","funded_amnt_inv", "term", "delinq_2yrs", "pub_rec")
test_model = model_3

test_predictions = makePredictions(test_model, real_test, test_predictors)

to_write = data.frame("member_id"=real_test$member_id, "loan_status"=test_predictions)
write.csv(to_write, file="LogReg.csv", row.names = FALSE, quote = FALSE)

## Score : 0.62