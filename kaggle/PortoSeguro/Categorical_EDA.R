library(data.table)
library(ggplot2)
library(corrplot)
library(grid)



###### Removing tables() ######
rm(list=tables()$NAME)
gc()



### Reading Data ###
eda_train = fread("data/train.csv")
eda_test = fread("data/test.csv")
cols = colnames(eda_train)



### Categorical Attributes ###
cat_cols = cols[grepl("_cat", cols)]
cat_cols

eda_cols  = c("id", cat_cols, "target")
eda_train = eda_train[, eda_cols, with=FALSE]
eda_test = eda_test[, c("id", cat_cols), with=FALSE]



### Bar Plot & Summary ###
getSummaryAndBarPlot = function(i){
  
  col = cat_cols[i]
  
  print(summary(eda_train[, cat_cols[i], with=FALSE]))
  
  #print(table(unlist(eda_train[, cat_cols[i], with=FALSE]), eda_train$target))
  
  print(eda_train[, .( count=.N, 
                       percent=.N/dim(eda_train)[1], 
                       one=sum(target==1),
                       one_percent=sum(target==1)/.N,
                       zero=sum(target==0),
                       zero_percent=sum(target==0)/.N
                    ),  
                  by=get(cat_cols[i])][order(get)])
  
  ggplot(eda_train, aes(as.factor(get(col)), fill=as.factor(target))) +
    geom_bar() + xlab(cat_cols[i])    
}

cat_cols
getSummaryAndBarPlot(14)
