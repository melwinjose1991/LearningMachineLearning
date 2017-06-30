library(xgboost)



missing_version = "1"
eng_version = "2"
train = read.csv(paste0("data/train_",missing_version, "_", eng_version, ".csv"), header=TRUE, sep=",")
test = read.csv(paste0("data/test_",missing_version, "_", eng_version, ".csv"), header=TRUE, sep=",")

names(train)

# Goal_Star
train_goal = read.csv(paste0("data/train_goal.csv"), header=TRUE, sep=",")
test_goal = read.csv(paste0("data/test_goal.csv"), header=TRUE, sep=",")
train = cbind(train, train_goal["goal_star"])
test = cbind(test, test_goal["goal_star"])
rm(train_goal)
rm(test_goal)
gc()

# Word2Vec
train_w2v = read.csv(paste0("data/train_w2v_500.csv"), header=TRUE, sep=",")
train = cbind(train, train_w2v)
rm(train_w2v)
gc()

test_w2v = read.csv(paste0("data/test_w2v_500.csv"), header=TRUE, sep=",")
test = cbind(test, test_w2v)
rm(test_w2v)
gc()


# tf-idf
train_tfidf = read.csv(paste0("data/train_tfidf.csv"), header=TRUE, sep=",")
test_tfidf = read.csv(paste0("data/test_tfidf.csv"), header=TRUE, sep=",")

train = cbind(train, tfidf_sum=train_tfidf$tfidf_sum)
test = cbind(test, tfidf_sum=test_tfidf$tfidf_sum)

rm(train_tfidf)
rm(test_tfidf)
gc()



## Mapping
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

map = getMap(train$country, test$country)
train$country_star = unlist(lapply(train$country, function(x) getFromMap(as.character(x), map)))
test$country_star = unlist(lapply(test$country, function(x) getFromMap(as.character(x), map)))

map = getMap(train$currency, test$currency)
train$currency_star = unlist(lapply(train$currency, function(x) getFromMap(as.character(x), map)))
test$currency_star = unlist(lapply(test$currency, function(x) getFromMap(as.character(x), map)))

map = getMap(train$state_change_after_deadline, test$state_change_after_deadline)
train$state_change_after_deadline = unlist(lapply(train$state_change_after_deadline, function(x) getFromMap(as.character(x), map)))
test$state_change_after_deadline = unlist(lapply(test$state_change_after_deadline, function(x) getFromMap(as.character(x), map)))

map = getMap(train$disable_communication, test$disable_communication)
train$disable_communication_star = unlist(lapply(train$disable_communication, function(x) getFromMap(as.character(x), map)))
test$disable_communication_star = unlist(lapply(test$disable_communication, function(x) getFromMap(as.character(x), map)))



## X  & Y
desc_words_cols = names(train)[grepl("desc_",names(train))]
desc_words_cols = desc_words_cols[!grepl("desc_starstar", desc_words_cols)]

keywords_words_cols = names(train)[grepl("keywords_",names(train))]
keywords_words_cols = keywords_words_cols[!grepl("keywords_starstar", keywords_words_cols)]

w2v_cols = names(train)[grepl("w2v_",names(train))]

tfidf_cols = names(train)[grepl("tfidf_",names(train))]

x = c(
  "country_star",
  #"currency_star",
  "disable_communication_star",
  "state_change_after_deadline",
  
  "deadline",
  "deadline_day",
  "deadline_month",
  "deadline_year",
  
  #"state_changed_at",
  "state_changed_at_day",
  "state_changed_at_month",
  #"state_changed_at_year",
  
  "created_at",
  "created_at_day",
  "created_at_month",
  #"created_at_year",
  
  "launched_at",
  "launched_at_day",
  "launched_at_month",
  #"launched_at_year",
  
  "deadline_created_in_sec_log",
  #"deadline_created_in_min",
  #"deadline_created_in_hour",
  #"deadline_created_in_day",
  
  "deadline_launched_in_sec",
  #"deadline_launched_in_min",
  #"deadline_launched_in_hour",
  #"deadline_launched_in_day",
  
  "deadline_state_changed_in_sec",
  #"deadline_state_changed_in_min",
  #"deadline_state_changed_in_hour",
  #"deadline_state_changed_in_day",
  
  "launch_created_in_sec_log",
  
  #"goal",
  "goal_star_log",
  "name_length",
  "desc_length",
  "keywords_length",
  #"dashes"
  
  # Encodings
  desc_words_cols,
  #keywords_words_cols,
  w2v_cols,
  tfidf_cols
  
)

#x = desc_words_cols
y = "final_status"

## NOTE : Double the final_status=1
train = rbind(train, train[train$final_status==1,])
sort( sapply(ls(),function(x){object.size(get(x))}))
gc()



### Ensemble of xgboost
i=1
final_predictions = rep(0, dim(test)[1])
seeds = c(1234,2345,3456)

for(seed in seeds){
  
  print(paste0("Xgboost#",i))
  
  rows = dim(train)[1]
  train_rows = sample(1:rows, 0.80*rows, replace=F)
  
  train_DM <- xgb.DMatrix(data = as.matrix(train[train_rows,x]), label=train[train_rows,y])
  valid_DM <- xgb.DMatrix(data = as.matrix(train[-train_rows,x]), label=train[-train_rows,y])
  
  seed_used = seed
  param = list(  objective           = "binary:logistic", 
                 booster             = "gbtree",
                 eta                 = 0.0125,
                 max_depth           = as.integer(length(x)),
                 min_child_weight    = 75,
                 subsample           = 1,
                 colsample_bytree    = 0.70,
                 seed                = seed_used
  )
  
  nrounds = 1250
  
  model = xgb.train(   params              = param, 
                       data                = train_DM,
                       nrounds             = nrounds, 
                       early_stopping_rounds  = 20,
                       watchlist           = list(val=valid_DM),
                       maximize            = FALSE,
                       eval_metric         = "error",
                       print_every_n = 25
  )

  ## Prediction
  test_DM <- xgb.DMatrix(data = as.matrix(test[,x]))
  
  test_pred = predict(model, test_DM)
  
  predictions = rep(0,dim(test)[1])
  predictions[test_pred>0.40] = 1 
  
  final_predictions = final_predictions + predictions
  #assign(paste0("pred_",i),predictions)

  i = i + 1
  
  rm(rows)
  rm(train_DM)
  rm(valid_DM)
  rm(test_DM)
  rm(model)
  rm(test_pred)
  #rm(predictions)
  gc()

}


## Majority Voting
predictions = ifelse(final_predictions>=(length(seeds)/2), 1, 0)


## Saving to file
pred = data.frame(project_id=test$project_id, final_status=predictions)
write.csv(pred, "submission_ensemble.csv", row.names = FALSE, quote=FALSE)
