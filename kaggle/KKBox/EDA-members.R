### Exploratory Data Analysis

library(data.table)
library(ggplot2)


###### Removing tables() ######
rm(list=tables()$NAME)
gc()



###### reading the csv files ######
eda_train_data = fread("data/train.csv")
eda_members_data = fread("data/members.csv")

head(eda_train_data)
head(eda_members_data)

str(eda_train_data)
str(eda_members_data)


setkey(eda_train_data, msno)
setkey(eda_members_data, msno)



###### Utility Functions ######
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



merged_dt = merge(eda_train_data, eda_members_data, all.x=TRUE)



###### city ######
tmp = merged_dt[ , .(count=.N, churns=sum(is_churn), percent_churns=sum(is_churn)/.N), by=city][order(-count)]
tmp

ggplot(merged_dt, aes(as.factor(city), fill=as.factor(is_churn))) + geom_bar()



###### age ######
sort(unique(merged_dt$bd))