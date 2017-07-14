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