
train = read.csv("data/train.csv", header=TRUE, sep=",")
test = read.csv("data/test.csv", header=TRUE, sep=",")

names(train)



## name : string
sum(is.na(train$name))
sum(train$name=="") # 1

sum(is.na(test$name))
sum(test$name=="")  # 0



## desc : string
sum(is.na(train$desc))
sum(train$desc=="") # 2

sum(is.na(test$desc))
sum(test$desc=="")  # 1



## Goal : int
summary(train$goal)
summary(test$goal)



## keywords : string : all unique
sum(is.na(train$keywords))
sum(train$keywords=="")

sum(is.na(test$keywords))
sum(test$keywords=="")



## disable_communication - categorical : (YES/NO)
summary(train$disable_communication)
summary(test$disable_communication)



## country - categorical
summary(train$country)
summary(test$country) # more countries here


names(train)
## currency - categorical
summary(train$currency)
summary(test$currency) # more currencies here



## deadline - numeric
summary(train$deadline)
summary(test$deadline)



## status_changed_at
summary(train$state_changed_at)
summary(test$state_changed_at)



## created_at : numeric time
summary(train$created_at)
summary(train$created_at)



## launched_at : numeric time
summary(train$launched_at)
summary(train$launched_at)


## backers_count :
summary(train$backers_count)
sum(train$backers_count==0) # 12832 zeros : outliers / missing ?

summary(test)
