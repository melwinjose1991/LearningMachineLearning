predict_func = function(fit, newdata, id){
  form = as.formula("orders_rcvd~.")
  mat = model.matrix(form, newdata)
  coefs = coef(fit, id=id)
  xvars = names(coefs)
  mat[, xvars]%*%coefs
}

cvSubsetSelection = function(df, no_vars, method="forward"){
  #df = data
  #no_vars = dim(df)[2]+11
  
  rows = nrow(df)
  print(paste0("Variables:", no_vars))
  
  k=10
  set.seed(1)
  folds = sample(1:k, rows, replace=TRUE)
  cv.errors = matrix(NA, k, no_vars)
  
  for(j in 1:k){
    #j=1
    best.fit = regsubsets(orders_rcvd~., data=df[folds!=j,], nvmax=no_vars, method=method)
    
    no_of_models = length(summary(best.fit)$rss)
    for(i in 1:no_of_models){
      #i=1
      #print(i)
      pred = predict_func(best.fit, df[folds==j,], id=i)
      cv.errors[j, i] = mean( abs(df$orders_rcvd[folds==j]-pred) )
    }
    
    id = which.min(cv.errors[j,])
    vars = names(coef(best.fit, id=id))
    #print(vars[2:length(vars)])
    print(vars)
  }
  
  mean.cv.errors = apply(cv.errors, 2, mean)
  par(mfrow=c(1,1))
  plot(mean.cv.errors, type='b')
  mean.cv.errors
}
