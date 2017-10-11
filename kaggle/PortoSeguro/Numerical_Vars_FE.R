library(data.table)



###### Removing tables() ######
rm(list=tables()$NAME)
gc()



### Reading Data ###
fe_train = fread("data/train.csv")
fe_test = fread("data/test.csv")
cols = colnames(fe_train)

setkey(fe_train, id)
setkey(fe_test, id)



### Numerical Attributes ###
numeric_cols = cols[which(sapply(fe_train, class) == "numeric")]
numeric_cols



### Counting -1s ###
fe_train[, num_neg_one := sum(.SD==-1), by=id, .SDcols=numeric_cols]
 fe_test[, num_neg_one := sum(.SD==-1), by=id, .SDcols=numeric_cols]



### Replacing -1 with mean ###
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



### Automated Feature Engineering after replacement ###
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

#funcs = list(XaddY=fXAddY, XminusY=fXMinusY, YminusX=fYMinusX, XmultiY=fXMinusY)
funcs = list(XmultiY=fXMinusY)
auto_engineered_without_missing = vector('character')

dim(fe_train)
for(i in 1:(length(numeric_cols)-1) ){
  for(j in (i+1):length(numeric_cols)){
    
    left_col = numeric_cols[i]
    right_col = numeric_cols[j]
    
    for(operation in names(funcs)){
      
      new_col = paste0(left_col, "_", operation, "_", right_col)
      auto_engineered_without_missing = c(auto_engineered_without_missing, new_col)
      
      fe_train[, (new_col) := mapply(funcs[[operation]], 
                                     fe_train[,left_col,with=FALSE], fe_train[,right_col, with=FALSE]) ]
      
      fe_test[, (new_col) := mapply(funcs[[operation]], 
                                     fe_test[,left_col,with=FALSE], fe_test[,right_col, with=FALSE]) ]
    
    }
  }
}
dim(fe_train)



### Engineered Features ###
cols_to_write = c("id", numeric_cols, "num_neg_one",
                  auto_engineered_without_missing)



###### PCA ######
cols_for_pca = c(numeric_cols, auto_engineered_without_missing)
combined = rbind(fe_train[,cols_for_pca,with=FALSE], 
                 fe_test[,cols_for_pca,with=FALSE])

train_n_rows = dim(fe_train)[1]
train_ids = fe_train$id
train_target = fe_train$target
rm(fe_train)

test_n_rows = dim(fe_test)[1]
test_ids = fe_test$id
rm(fe_test)

gc()


pca = prcomp(combined, scale.=TRUE)
new_dim = as.matrix(combined) %*% as.matrix(pca$rotation[,1:10])

fe_train = as.data.frame(new_dim[1:train_n_rows,])
fe_train[,"id"] = train_ids
fe_train[,"target"] = train_target

fe_test = as.data.frame(new_dim[(train_n_rows+1):dim(new_dim)[1],])
fe_test[,"id"] = test_ids

fwrite(fe_train, "data/afe_train_pca_1.csv", 
       quote=FALSE, sep=",", row.names=FALSE)

fwrite(fe_test, "data/afe_test_pca_1.csv", 
       quote=FALSE, sep=",", row.names=FALSE)



### Writing ###
tables()

fwrite(fe_train[,c(cols_to_write,"target"),with=FALSE], 
       "data/afe_train_1.csv", quote=FALSE, sep=",", row.names=FALSE)

fwrite(fe_test[,cols_to_write, with=FALSE], 
       "data/afe_test_1.csv", quote=FALSE, sep=",", row.names=FALSE)



###### Removing tables() ######
rm(list=tables()$NAME)
gc()


