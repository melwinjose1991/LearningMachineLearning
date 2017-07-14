library(caret)


removeUnchangingVariables = function(data){
  data[sapply(data, function(x) length(unique(x)) > 1)]
}



removeCorrelatedVariables = function(data, corr_threshold=0.99) {
  
  data_num_var = data[, !names(data) %in% c("month", product_data_column, "t")]
  
  tmp = cor(data_num_var)
  tmp[!lower.tri(tmp)] = 0
  
  uncorrelated_vars = names(data_num_var[, !apply(tmp, 2, function(x) any(x > corr_threshold))])
  # print("UnCorrelated Vars")
  # print(uncorrelated_vars)
  
  correlated_vars = names(data_num_var[, apply(tmp, 2, function(x) any(x > corr_threshold))])
  print(paste0("Correlated Vars : ",length(correlated_vars)))
  print(correlated_vars)
  
  data.new = data[, c(product_data_column, "month", "t", uncorrelated_vars)]
  
  data.new
  
}



removeCorrelatedVariablesCARET = function(data, corr_threshold=0.99){
  
  data_num_var = data[, !names(data) %in% c("month", product_data_column, "t")]

  corr_df = cor(data_num_var)
  
  high_corr_cols = findCorrelation(corr_df, cutoff=corr_threshold)
  correlated_vars = names(data_num_var)[high_corr_cols]
  print(paste0("Correlated Vars : ",length(correlated_vars)))
  print(correlated_vars)
  
  uncorrelated_cols = names(data_num_var)[-high_corr_cols]
  # print(uncorrelated_vars)
  
  data.new = data[,c("orders_rcvd","month", "t", uncorrelated_cols)]
}