library(glmnet)



missing_version = "1"
eng_version = "2"
train = read.csv(paste0("data/train_",missing_version, "_", eng_version, ".csv"), header=TRUE, sep=",")
test = read.csv(paste0("data/test_",missing_version, "_", eng_version, ".csv"), header=TRUE, sep=",")

names(train)



## Add missing levels
cols = c("country","currency")

for(col in cols){
  total_levels = union(unique(train[,col]), unique(test[,col]))
  new_col = paste0(col,"_star")
  train[,new_col] = factor(train[,col], levels=total_levels)
  test[,new_col] = factor(test[,col], levels=total_levels)
  print(sum(as.character(train[,col])!=as.character(train[,new_col])))
  print(sum(as.character(test[,col])!=as.character(test[,new_col])))
}



## X & Ys
if(FALSE){
categorical_vars = "country_star + currency_star + state_change_after_deadline"
numerical_vars = "state_changed_at + created_at + launched_at + deadline_created 
                  + deadline_launched + deadline_state_changed + goal_star
                  + name_length + desc_length + keywords_length + dashes"
}

categorical_vars = "country_star + state_change_after_deadline + disable_communication"
categorical_vars = paste0(categorical_vars, " + " ,paste(d_, collapse=" + "))

numerical_vars = paste0("state_changed_at + created_at + launched_at + deadline_created", 
                        "+ deadline_launched + deadline_state_changed + goal_star",
                        "+ name_length + desc_length + keywords_length + dashes")
predictor_var = "final_status"

train_categorical_form = as.formula( paste0(predictor_var," ~ ", categorical_vars) )
train_numerical_form = as.formula( paste0(predictor_var," ~ ", numerical_vars) )



## Hyper-Parameter tuning
alpha = 1

train_rows = sample(1:nrow(train), 0.5*nrow(train), replace=FALSE)
train_star = train[train_rows,]

train_x_categorical = model.matrix(train_categorical_form, data=train_star)[,-1]
train_x_numericals = model.matrix(train_numerical_form, data=train_star)[,-1]

train_x = as.matrix(data.frame(train_x_numericals, train_x_categorical))
train_y = train_star$final_status

grid = 2.71828^seq(-80, 80, length=16)
cv.l2.fit = cv.glmnet(train_x, train_y, alpha=alpha, family="binomial", lambda=grid, nfolds=5)
plot(cv.l2.fit)

best_lambda = cv.l2.fit$lambda.min
best_lambda
log(best_lambda)
best_lambda_index = match(best_lambda, cv.l2.fit$lambda)
best_lambda_index

cv.l2.fit$cvm[best_lambda_index]
# 1.23 - 0.659 - alpha=1
# 1.19 - 0.659 - alpha=1
# 1.18 - 0.659 - alpha=1
# 1.15 - 0.660 - alpha=0.5



## Training Model
train_star = train

train_x_categorical = model.matrix(train_categorical_form, data=train_star)[,-1]
train_x_numericals = model.matrix(train_numerical_form, data=train_star)[,-1]

train_x = as.matrix(data.frame(train_x_numericals, train_x_categorical))
train_y = train_star$final_status

l2.fit = glmnet(train_x, train_y, alpha=alpha, family="binomial", lambda=best_lambda)
coef(l2.fit)



## Predictions
test_categorical_form = as.formula( paste0("state_changed_at ~ ", categorical_vars) )
test_numerical_form = as.formula( paste0("country ~ ", numerical_vars) )

test_x_categorical = model.matrix(test_categorical_form, data=test)[,-1]
test_x_numericals = model.matrix(test_numerical_form, data=test)[,-1]

test_x = as.matrix(data.frame(test_x_numericals, test_x_categorical))

pred_prob = predict(l2.fit, newx=test_x)
predictions = rep(0, dim(test)[1])

predictions[pred_prob>0.5] = 1

pred = data.frame(project_id=test$project_id, final_status=predictions)
write.csv(pred, "submission_regression.csv", row.names = FALSE, quote=FALSE)
  