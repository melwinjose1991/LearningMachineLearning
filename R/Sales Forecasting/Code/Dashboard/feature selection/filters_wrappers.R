library(caret)
library(party)
library(Boruta)



removeUnchangingVariables = function(data){
  data[sapply(data, function(x) length(unique(x)) > 1)]
}



removeCorrelatedVariables = function(data, corr_threshold=0.99) {
  print("Fiter Features :: removeCorrelatedVariables() :: START")
  
  data_num_var = data[, !names(data) %in% c("month", product_data_column, "t")]
  
  tmp = cor(data_num_var)
  tmp[!lower.tri(tmp)] = 0
  
  uncorrelated_vars = names(data_num_var[, !apply(tmp, 2, function(x) any(x > corr_threshold))])
  # print("UnCorrelated Vars")
  # print(uncorrelated_vars)
  
  correlated_vars = names(data_num_var[, apply(tmp, 2, function(x) any(x > corr_threshold))])
  print(paste0("Correlated Vars : ",length(correlated_vars)))
  print(correlated_vars)
  
  print("Fiter Features :: removeCorrelatedVariables() :: END")
  data[, c(product_data_column, "month", "t", uncorrelated_vars)]
  
}



removeCorrelatedVariablesCARET = function(data, corr_threshold=0.99){
  print("Fiter Features :: removeCorrelatedVariablesCARET() :: START")
  
  data_num_var = data[, !names(data) %in% c("month", product_data_column, "t")]

  corr_df = cor(data_num_var)
  
  high_corr_cols = findCorrelation(corr_df, cutoff=corr_threshold)
  correlated_vars = names(data_num_var)[high_corr_cols]
  print(paste0("Correlated Vars : ",length(correlated_vars)))
  print(correlated_vars)
  
  uncorrelated_cols = names(data_num_var)[-high_corr_cols]
  # print(uncorrelated_vars)
  
  print("Fiter Features :: removeCorrelatedVariablesCARET() :: END")
  data[,c(product_data_column, "month", "t", uncorrelated_cols)]

}


getImportantVarCIForest = function(data){
  print("Fiter Features :: getImportantVarCIForest() :: START")
  
  data_num_vars = data[,!names(data) %in% c("month", "t")]
  form = as.formula(paste0(product_data_column,"~."))
  cf1 = cforest(form, data=data_num_vars, controls=cforest_unbiased(mtry=2, ntree=50))
  vi = varimp(cf1, conditional=TRUE)
  
  unsignificant_vars = names(vi)[vi<=0]
  print(paste0("Non-Significant Variables : ",length(unsignificant_vars)))
  
  significant_vars = names(vi)[vi>0]
  #length(significant_vars)
  
  print("Fiter Features :: getImportantVarCIForest() :: END")
  data[,c(product_data_column, "month", "t", significant_vars)]
  
}

getImportantVarBoruta = function(data){
  print("Fiter Features :: getImportantVarBoruta() :: START")
  
  data_num_vars = data[,!names(data) %in% c("month", "t")]
  
  form = as.formula(paste0(product_data_column,"~."))
  output = Boruta(form, data=data_num_vars)

  unsignificant_vars = names(output$finalDecision[output$finalDecision %in% c("Rejected")])
  print(paste0("Non-Significant Variables : ",length(unsignificant_vars)))
    
  significant_vars = names(output$finalDecision[output$finalDecision %in% c("Confirmed", "Tentative")])
  #length(significant_vars)
  
  print("Fiter Features :: getImportantVarBoruta() :: END")
  data[,c(product_data_column,"month", "t", significant_vars)]
  
}

doLassoPlusVif = function(data, max_iter=20, nfolds=24, error_type="mae", vif_threshold=4){
  print("Fiter Features :: doLASSOPlusVif() :: START")
  
  grid = 2.71828^seq(0.001, 9, length=1000)
  tmp_data = data

  i=1
  while(1){
    
    print(paste0("Total Vars : ", (dim(tmp_data)[2]-1)))
    
    form = as.formula(paste0(product_data_column,"~."))
    x = model.matrix(form, tmp_data)
    y = tmp_data[,product_data_column]
    
    cv.l2.fit = cv.glmnet(x, y, alpha=1, type.measure=error_type, lambda=grid, nfolds=nfolds)
    # plot(cv.l2.fit)
    
    ## best model
    best_lambda = cv.l2.fit$lambda.min
    #best_lambda
    best_lambda_index = match(best_lambda, cv.l2.fit$lambda)
    #best_lambda_index
    
    #cv.l2.fit$cvm[best_lambda_index]
    
    l2.fit = glmnet(x, y, alpha=1, lambda=best_lambda)
    coefs = coef(l2.fit)[,1]
    #coefs[coefs!=0]
    lasso_x = names(coefs[coefs!=0])[-1]
    
    df = as.data.frame(x[,lasso_x])
    df[, product_data_column] = y
    
    form = as.formula(paste0(product_data_column,"~."))
    fit = lm(form, df)
    vifs = vif(fit)
    
    remove_index = which.max(vifs)
    if(vifs[[remove_index]]>vif_threshold){
      remove_var = names(vifs[remove_index])
      print(paste0("Removed : ",remove_var, " VIF:", vifs[[remove_index]]))
      tmp_data = tmp_data[,!names(tmp_data) %in% remove_var]
      i = i+1
      if(i>max_iter){
        print(paste0("Max Iteration Reached : ", max_iter))
        #break;
      }
    }else{
      break;
    }
    
  }
  
  print("Fiter Features :: doLASSOPlusVif() :: DONE")
  tmp_data
}