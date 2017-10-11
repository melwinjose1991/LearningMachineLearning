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



### Categorical Attributes ###
cat_cols = cols[grepl("_cat", cols)]
cat_cols

fe_cols  = c("id", cat_cols, "target")
fe_train = fe_train[, fe_cols, with=FALSE]
fe_test = fe_test[, c("id", cat_cols), with=FALSE]



### Counting -1s ###
fe_train[, cat_neg_ones := sum(.SD==-1), by=id, .SDcols=cat_cols]
 fe_test[, cat_neg_ones := sum(.SD==-1), by=id, .SDcols=cat_cols]



### Replacing -1 with mean ###
if(FALSE){
  cols_with_neg1 = sapply(cat_cols, 
                          function(col){sum(fe_train[,col,with=FALSE]==-1)>1})
  cols_with_neg1 = cat_cols[cols_with_neg1]
  cols_with_neg1
  for(col in cols_with_neg1){
    mean_val = fe_train[fe_train[,get(col)]!=-1, mean(get(col))]
    fe_train[fe_train[,get(col)]==-1, eval(col) := mean_val]
    summary(fe_train[,col,with=FALSE])
  }
  
  cols_with_neg1 = sapply(cat_cols, 
                          function(col){sum(fe_test[,col,with=FALSE]==-1)>1})
  cols_with_neg1 = cat_cols[cols_with_neg1]
  cols_with_neg1
  for(col in cols_with_neg1){
    mean_val = fe_test[fe_test[,get(col)]!=-1, mean(get(col))]
    fe_test[fe_test[,get(col)]==-1, eval(col) := mean_val]
    summary(fe_test[,col,with=FALSE])
  }
}


### Automated Feature Engineering after replacement ###
## List of functions
fXAddY = function(x,y){
  
  if(x==-1 || y==-1){
    
    if(x==-1 && y==-1){
      return(-2)
    }else{
      return(-1)
    }
    
  }else{
    return(x+y)
  }
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
funcs = list(add=fXAddY)
auto_engineered_without_missing = vector('character')

dim(fe_train)
for(i in 1:(length(cat_cols)-1) ){
  for(j in (i+1):length(cat_cols)){
    
    left_col = cat_cols[i]
    right_col = cat_cols[j]
    
    print(paste0(left_col, " & ", right_col))
    
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
cols_to_write = c("id", cat_cols, "cat_neg_ones",
                  auto_engineered_without_missing)



### Writing ###
tables()

fwrite(fe_train[,c(cols_to_write,"target"),with=FALSE], 
       "data/afe_train_cat_1.csv", quote=FALSE, sep=",", row.names=FALSE)

fwrite(fe_test[,cols_to_write, with=FALSE], 
       "data/afe_test_cat_1.csv", quote=FALSE, sep=",", row.names=FALSE)
# reduced from 153 to ~5 seconds



###### Removing tables() ######
rm(list=tables()$NAME)
gc()


