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



rows = dim(train)[1]
train_rows = sample(1:rows, 0.80*rows, replace=F)

train_DM <- xgb.DMatrix(data = as.matrix(train[train_rows,x]), label=train[train_rows,y])
valid_DM <- xgb.DMatrix(data = as.matrix(train[-train_rows,x]), label=train[-train_rows,y])

setdiff(x, intersect(x, names(train)))



## Hyperparameter Tuning
rows = dim(train)[1]
rows
train_rows = sample(1:rows, 0.50*rows, replace=F)
train = train[train_rows, ]

rows = dim(train)[1]
rows
train_rows = sample(1:rows, 0.80*rows, replace=F)
train_DM <- xgb.DMatrix(data = as.matrix(train[train_rows,x]), label=train[train_rows,y])
valid_DM <- xgb.DMatrix(data = as.matrix(train[-train_rows,x]), label=train[-train_rows,y])

configs = vector('character') 
errors = vector('character')

rm(train)
rm(train_goal)
rm(test)
rm(test_goal)
rm(train_rows)
rm(getMap)
gc()
sort( sapply(ls(),function(x){object.size(get(x))}))

for(param_1 in c(75,100,125)){                      # min_child_weight
  for(param_2 in c(1,2,3)){                             # max_depth divider
    for(param_3 in c(0.8,0.9,1)){                     # subsample
      for(param_4 in c(0.6,0.7,0.8)){                   # colsample_bytree
        
        config = paste0("param1:", param_1, " and ", "param2:", param_2, 
                     " and ", "param3:", param_3," and ", "param4:", param_4)
        print(config)
        configs = c(configs, config)
                
        param = list(  objective           = "reg:logistic", 
                       booster             = "gbtree",
                       eta                 = 1, #0.25,
                       max_depth           = as.integer(length(x)/param_2),
                       min_child_weight    = param_1,
                       subsample           = param_3,
                       colsample_bytree    = param_4
        )
        
        nrounds = 100 # 250
        model = xgb.cv(      params              = param, 
                             data                = train_DM,
                             nrounds             = nrounds, 
                             nfold               = 3,
                             early_stopping_rounds  = 20,
                             watchlist           = list(val=valid_DM),
                             maximize            = FALSE,
                             eval_metric         = "error",
                             verbose             = FALSE
                             #print_every_n       = 50
        )
        error = model$evaluation_log[model$best_iteration]$test_error_mean
        print(error)
        
        errors = c(errors, error)
        
        rm(model)
        gc()
      }
    }
  }
}



## Training 
rows = dim(train)[1]
train_rows = sample(1:rows, 0.80*rows, replace=F)

train_DM <- xgb.DMatrix(data = as.matrix(train[train_rows,x]), label=train[train_rows,y])
valid_DM <- xgb.DMatrix(data = as.matrix(train[-train_rows,x]), label=train[-train_rows,y])

seed_used = 1234
param = list(  objective           = "binary:logistic", 
               booster             = "gbtree",
               eta                 = 0.0125,
               max_depth           = as.integer(length(x)/1),
               min_child_weight    = 75,
               subsample           = 1,
               colsample_bytree    = 0.70,
               seed                = seed_used
)

nrounds = 2000

model = xgb.train(   params              = param, 
                     data                = train_DM,
                     nrounds             = nrounds, 
                     early_stopping_rounds  = 20,
                     watchlist           = list(val=valid_DM),
                     maximize            = FALSE,
                     eval_metric         = "error",
                     print_every_n = 25
)

imp = as.data.frame(xgb.importance(feature_names = x, model = model))
imp

x = imp$Feature[1:429]


valid_pred = predict(model, valid_DM)
valid_df = train[-train_rows,]

predictions = rep(0,dim(valid_df)[1])
predictions[valid_pred>0.50] = 1 
sum(predictions==valid_df$final_status) / dim(valid_df)[1]


## Prediction
test_DM <- xgb.DMatrix(data = as.matrix(test[,x]))

test_pred = predict(model, test_DM)

predictions = rep(0,dim(test)[1])
predictions[test_pred>0.40] = 1 
# 0.55  = 0.73815
# 0.50  = 0.74694
# 0.45  = 0.75129
# 0.425 = 0.75246
# 0.415 = 0.75309
# 0.40  = 0.75347
# 0.37  = 0.75344
# 0.34 = 0.75117

which.max(sapply(1:6, function(a) summary(lm(y~poly(x,a), data=df))$r.squared))
# square is a good fit
fit = lm(y~poly(x,2), data=df)
xs = seq(0.37, 0.425, length=20)
which.max(sapply(xs, function(a) predict(fit, newdata=data.frame(x=a))))
xs[10]

pred = data.frame(project_id=test$project_id, final_status=predictions)
write.csv(pred, "submission_xgb_w2v500.csv", row.names = FALSE, quote=FALSE)


