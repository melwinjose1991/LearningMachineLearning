library(forecast)



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




getBenchmarkResults=function(y, model_predictions, h=6, 
                             mean_model=TRUE, naive_model=TRUE, snaive_model=TRUE,
                             drift_model=TRUE){
  
  # Source : https://www.otexts.org/fpp/2/3
  
  t = ts(y, frequency=12)
  plot(t)
  t_window = window(t, end=4+((12-(h+1))/12))
  
  train_indices = 1:(length(y)-h)
  y_actual = y[-train_indices]

  ## Average Method
  if(mean_model){
    predictions = meanf(t_window, h)$mean
    lines(predictions, col=2, lwd=2, lty=2)
    mae = mean( abs(predictions - y_actual) )
    print(paste0("Prediction MAE for Average Method : ", mae))
  }
  
  ## Naive Method
  if(naive_model){
    predictions = naive(t_window, h)$mean
    lines(predictions, col=3, lwd=2, lty=2)
    mae = mean( abs( predictions - y_actual) )
    print(paste0("Prediction MAE for Naive Method : ", mae))
  }
  
  ## Seasonal Naive Method
  if(snaive_model){
    predictions = snaive(t_window, h)$mean
    lines(predictions, col=4, lwd=2, lty=2)
    mae = mean( abs( predictions - y_actual) )
    print(paste0("Prediction MAE for Seasonal Naive Method : ", mae))
  }
  
  ## Drift Method
  if(drift_model){
    predictions = rwf(t_window, h, drift=TRUE)$mean
    lines(predictions, col=5, lwd=2, lty=2)
    mae = mean( abs( predictions - y_actual) )
    print(paste0("Prediction MAE for Drift Method : ", mae))
  }

  # Predictions by your model
  t_pred = ts(model_predictions, frequency = 12, start=4+((12-h)/12))
  lines(t_pred, col=6, lwd=2, lty=2)
  mae = mean( abs( model_predictions - y_actual) )
  print(paste0("Prediction MAE for ModelX : ", mae))
  
  legend("topleft", lwd=2, lty=2, col=c(2,3,4,5,9),
         legend=c("Mean","Naive","Seasonal Naive","Drift","ModelX"))
}


convertSeries = function(data_folder, product, file_to_convert, start_year, start_month
                         , frequency=12, write_to){
  
  product = "2404"
  file_to_convert = "tmp.csv"
  data_folder = paste0("../../Data/", product, "/")
  start_year = 2004
  start_month = 1
  frequency = 12
  write_to = "Revenue.csv"
  
  
  file_to_convert = paste0(data_folder, file_to_convert)
  old_df = read.csv(file_to_convert)
  
  index = 1
  month_i = start_month
  year_i = start_year
  
  new_df = old_df
  new_df[,"t"] = 1:nrow(new_df)
  new_df[,"month"] = sapply(new_df$t, function(x){
    ifelse(x%%frequency!=0, x%%frequency, frequency)
  })
  new_df[,"year"] = sapply(new_df$t, function(x){
    start_year + as.integer((x-1)/12)
  })
  new_df[,"period_id"] = paste0(new_df$month,"/",new_df$year)
  
  write.csv(new_df, paste0(data_folder, "/", write_to), quote=FALSE, row.names=FALSE)
  
}


