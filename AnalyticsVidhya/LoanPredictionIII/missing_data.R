train = read.csv("data/train.csv", header=TRUE, sep=",")
test = read.csv("data/test.csv", header=TRUE, sep=",")



names(train)



## Gender
summary(train$Gender)
# 13 ""
train[train$Gender=="","Gender"] = "Male"
train$Gender = factor(train$Gender)
summary(train$Gender)

summary(test$Gender)
# 11 ""
test[test$Gender=="","Gender"] = "Male"
test$Gender = factor(test$Gender)
summary(test$Gender)



## Married
summary(train$Married)
# 3 ""
train[train$Married=="","Married"] = "Yes"
train$Married = factor(train$Married)
summary(train$Married)

summary(test$Married)



## Dependents
summary(train$Dependents)
# 15 ""

summary(test$Dependents)
# 10 ""


## Education
summary(train$Education)
summary(test$Education)



## Self_Employed
summary(train$Self_Employed)
# 32 ""

summary(test$Self_Employed)
# 23 ""


## ApplicantIncome
summary(train$ApplicantIncome)
summary(test$ApplicantIncome)


## CoApplicantIncome
summary(train$CoapplicantIncome)
summary(test$CoapplicantIncome)



## LoanAmount
summary(train$LoanAmount)
# 22 Na's
median_loan_amt = median(c(train$LoanAmount, test$LoanAmount), na.rm=TRUE)
train[is.na(train$LoanAmount),"LoanAmount"] = median_loan_amt

summary(test$LoanAmount)
# 5 NA's
test[is.na(test$LoanAmount),"LoanAmount"] = median_loan_amt



## Loan_Amount_Term
summary(train$Loan_Amount_Term)
# 14 NA's
median_loan_term = median(c(train$Loan_Amount_Term, test$Loan_Amount_Term), na.rm=TRUE)
train[is.na(train$Loan_Amount_Term),"Loan_Amount_Term"] = median_loan_term

summary(test$Loan_Amount_Term)
# 6 NA's
test[is.na(test$Loan_Amount_Term),"Loan_Amount_Term"] = median_loan_term



## Credit History
summary(train$Credit_History)
# 50 NA's
median_credit_hist = median(c(train$Credit_History, test$Credit_History), na.rm=TRUE)
train[is.na(train$Credit_History),"Credit_History"] = median_credit_hist
train$Credit_History = as.factor(train$Credit_History)

summary(test$Credit_History)
# 29 NA's
test[is.na(test$Credit_History),"Credit_History"] = median_credit_hist
test$Credit_History = as.factor(test$Credit_History)



## Property_Area
summary(train$Property_Area)
summary(test$Property_Area)



## Saving
write.csv(train, "data/train_1.csv", row.names = FALSE)
write.csv(test, "data/test_1.csv", row.names = FALSE)

# 1 : Majority or Median or Default values
