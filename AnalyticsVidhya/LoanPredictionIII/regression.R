library(glmnet)

train = read.csv("data/train_1.csv", header=TRUE, sep=",")
test = read.csv("data/test_1.csv", header=TRUE, sep=",")




## X
x_factors = model.matrix(Loan_Status ~ Gender + Married + Education + Credit_History 
                         + Property_Area, data=train)[,-1]
x_numericals = model.matrix(Loan_Status ~ ApplicantIncome + CoapplicantIncome 
                            + LoanAmount + Loan_Amount_Term, data=train)[,-1]
x = as.matrix(data.frame(x_numericals, x_factors))

y = as.factor(train$Loan_Status)


## CV Lasso Regression
grid = 2.71828^seq(-6, 1, length=1000)
cv.l2.fit = cv.glmnet(x, y, alpha=1, family="binomial", lambda=grid)
plot(cv.l2.fit)

best_lambda = cv.l2.fit$lambda.min
best_lambda
best_lambda_index = match(best_lambda, cv.l2.fit$lambda)
best_lambda_index

cv.l2.fit$cvm[best_lambda_index]
# 1.24 - 0.708
# 0.96 - 0.778
# 0.95 - 0.784

l2.fit = glmnet(x, y, alpha=1, family="binomial", lambda=best_lambda)
coef(l2.fit)



## Prediction
x_factors = model.matrix(ApplicantIncome ~ Gender + Married + Education + Credit_History 
                         + Property_Area, data=test)[,-1]
x_numericals = model.matrix(Gender ~ ApplicantIncome + CoapplicantIncome + LoanAmount 
                            + Loan_Amount_Term, data=test)[,-1]
x = as.matrix(data.frame(x_numericals, x_factors))

pred_prob = predict(l2.fit, newx=x)
predictions = rep("N", dim(test)[1])

predictions[pred_prob>0.5] = "Y"

pred = data.frame(Loan_ID=test$Loan_ID, Loan_Status=predictions)
write.csv(pred, "submission_regression.csv", row.names = FALSE)

