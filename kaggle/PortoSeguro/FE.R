library(data.table)



###### Removing tables() ######
rm(list=tables()$NAME)
gc()



### Reading Data ###
fe_train = fread("data/train.csv")
fe_test = fread("data/test.csv")
cols = colnames(fe_train)



### Automated Feature Engineering - Numeric Attributes ###
numeric_cols = cols[which(sapply(fe_train, class) == "numeric")]
numeric_cols

cols_with_neg1 = sapply(numeric_cols, function(col){sum(fe_train[,col,with=FALSE]==-1)>1})
cols_with_neg1 = numeric_cols[cols_with_neg1]
cols_with_neg1
for(col in cols_with_neg1){
  mean_val = fe_train[fe_train[,get(col)]!=-1, mean(get(col))]
  fe_train[fe_train[,get(col)]==-1, eval(col) := mean_val]
  summary(fe_train[,col,with=FALSE])
}

cols_with_neg1 = sapply(numeric_cols, function(col){sum(fe_test[,col,with=FALSE]==-1)>1})
cols_with_neg1 = numeric_cols[cols_with_neg1]
cols_with_neg1
for(col in cols_with_neg1){
  mean_val = fe_test[fe_test[,get(col)]!=-1, mean(get(col))]
  fe_test[fe_test[,get(col)]==-1, eval(col) := mean_val]
  summary(fe_test[,col,with=FALSE])
}


## List of functions
fXAddY = function(x,y){
  x+y
}

fXMinusY = function(x,y){
  x-y
}

fYMinusX = function(x,y){
  y-x
}

fXMultY = function(x,y){
  x*y
}

#funcs = list(add=fXAddY, XminusY=fXMinusY, YminusX=fYMinusX, XmultiY=fXMinusY)
funcs = list(add=fXAddY, XmultiY=fXMinusY)
engineered_num_features = vector('character')

for(i in 1:(length(numeric_cols)-1) ){
  for(j in (i+1):length(numeric_cols)){
    
    left_col = numeric_cols[i]
    right_col = numeric_cols[j]
    
    for(operation in names(funcs)){
      
      new_col = paste0(left_col, "_", operation, "_", right_col)
      engineered_num_features = c(engineered_num_features, new_col)
      
      fe_train[, (new_col) := mapply(funcs[[operation]], 
                                     fe_train[,left_col,with=FALSE], fe_train[,right_col,with=FALSE]) ]
      
      fe_test[, (new_col) := mapply(funcs[[operation]], 
                                     fe_test[,left_col,with=FALSE], fe_test[,right_col,with=FALSE]) ]
    
    }
  }
}


### Engineered Features ###
cols_to_write = c("id", numeric_cols, engineered_num_features)


### Writing ###
tables()

write.table(fe_train[,c(cols_to_write,"target"),with=FALSE], 
            "data/fe_train_1.csv", quote=FALSE, sep=",", row.names=FALSE)

write.table(fe_test[,cols_to_write, with=FALSE], 
            "data/fe_test_1.csv", quote=FALSE, sep=",", row.names=FALSE)
