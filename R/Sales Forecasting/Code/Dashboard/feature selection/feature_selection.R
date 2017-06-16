library(glmnet)



## Globals
featureSelection_prefix = paste0(feature_selection_prefix, "featureSelection_")

data_folder = "../../Data/"
revenue_file = paste0(data_folder, product, "/", product, "_Revenue.csv")
sa_OR_nsa = "Not Seasonally Adjusted"



## UI Elements
initFeatureSelectionUI = function() {
  # row 1
  text_advanced_text = tags$h4("Advanced Options")
  
  row_1_col_width = 3
  row_1_textInput_width = '40%'
  text_lambda = column(row_1_col_width, tags$h5("lambda"))
  text_lambda_start = column(
    row_1_col_width,
    textInput(
      paste0(featureSelection_prefix, "lambda_start"),
      "lambda start",
      value = "0.001",
      width = row_1_textInput_width
    )
  )
  text_lambda_end = column(
    row_1_col_width,
    textInput(
      paste0(featureSelection_prefix, "lambda_end"),
      "lambda end value",
      value = "10",
      width = row_1_textInput_width
    )
  )
  text_lambda_lengths = column(
    row_1_col_width,
    textInput(
      paste0(featureSelection_prefix, "lambda_length"),
      "lengths",
      value = "100",
      width = row_1_textInput_width
    )
  )
  select_error_type =  column(row_1_col_width, selectInput(
    paste0(featureSelection_prefix, "selectErrorType"),
    "Error Type:",
    c(
      "Mean Square Error" = "mse",
      "Mean Absolute Error" = "mae"
    )
  ))
  
  row_1 = fluidRow(
    text_advanced_text,
    #text_lambda,
    text_lambda_start,
    text_lambda_end,
    text_lambda_lengths,
    select_error_type
  )
  
  # row 2
  button_run_LASSO = actionButton(inputId = paste0(featureSelection_prefix, "buttonLASSO"),
                                  label = "Run LASSO")
  row_2 = fluidRow(button_run_LASSO)
  
  # row 3
  output_graph = plotOutput(paste0(featureSelection_prefix, "outputLASSOGraph"))
  row_3 = fluidRow(output_graph)
  
  # row 4
  output_coefs = uiOutput(paste0(featureSelection_prefix, "outputLASSOCoefs"))
  row_4 = fluidRow(output_coefs)
  
  
  tabFeatureSelection = tabPanel(title = "Feature Selection",
                                 row_1,
                                 row_2,
                                 row_3,
                                 row_4)
  
  tabFeatureSelection
}



## Helper Functions
readData = function(inputs) {
  print("Feature Selection :: Feature Selection :: readData() :: INIT")
  
  #inputs = inputs[lapply(inputs, is.logical)==TRUE]
  selected_vars = vector('character')
  for (key in names(inputs)) {
    if (is.logical(inputs[[key]]) & inputs[[key]] == TRUE) {
      selected_vars = c(selected_vars, unlist(strsplit(key, "\\|"))[2])
    }
  }
  print("All features that were selected")
  print(selected_vars)
  
  config_data = meta_data[meta_data$series_id %in% selected_vars, ]
  
  # Y
  data_revenue = read.csv(revenue_file, header = TRUE, sep = ",")
  data = data_revenue[, c("orders_rcvd", "month", "t")]
  data$month = as.factor(data$month)
  
  # X
  for (sub_category_id in unique(config_data$sub_category_id)) {
    category_name = unique(config_data[config_data$sub_category_id == sub_category_id, "category_name"])
    sub_category_name = unique(config_data[config_data$sub_category_id ==
                                             sub_category_id, "sub_category_name"])
    
    
    file = paste0(
      data_folder,
      product,
      "/",
      as.character(category_name),
      "/",
      as.character(sub_category_name)
    )
    
    if (sa_OR_nsa == "Not Seasonally Adjusted") {
      file = paste0(file, "_nsa.csv")
    } else{
      file = paste0(file, "_sa.csv")
    }
    print(paste0("Reading file : ", file))
    data_series = read.csv(file, header = TRUE, sep = ",")
    
    #data_series = data_series[, !names(data_series) %in% c("date")]
    series_vars = intersect(names(data_series), selected_vars)
    
    data = cbind(data, data_series[, series_vars])
    
  }
  
  print("Feature Selection :: Feature Selection :: readData() :: EXIT")
  filterFeatures(data, inputs)
  
}



filterFeatures = function(data, inputs) {
  print("Feature Selection :: Feature Selection :: filterFeatures() :: INIT")
  
  # Removing columns whose values don't change
  data = data[sapply(data, function(x)
    length(unique(x)) > 1)]
  
  # Removing highly correlated variables
  data_num_var = data[, !names(data) %in% c("month", "orders_rcvd", "t")]
  tmp = cor(data_num_var)
  tmp[!lower.tri(tmp)] = 0
  uncorrelated_vars = names(data_num_var[, !apply(tmp, 2, function(x)
    any(x > 0.99))])
  print("UnCorrelated Vars")
  print(uncorrelated_vars)
  
  data.new = data[, c("orders_rcvd", "month", "t", uncorrelated_vars)]
  
  print("Feature Selection :: Feature Selection :: filterFeatures() :: EXIT")
  doLASSO(data.new, inputs)
}



doLASSO = function(data, inputs) {
  print("Feature Selection :: Feature Selection :: doLASSO() :: INIT")
  
  lambda_start = as.numeric(inputs[paste0(featureSelection_prefix, "lambda_start")])
  lambda_end = as.numeric(inputs[paste0(featureSelection_prefix, "lambda_end")])
  lambda_length = as.integer(inputs[paste0(featureSelection_prefix, "lambda_length")])
  error_type = as.character(inputs[paste0(featureSelection_prefix, "selectErrorType")])
  
  grid = 2.71828 ^ seq(lambda_start, lambda_end, length = lambda_length)
  
  x = model.matrix(orders_rcvd ~ ., data)[, -1]
  y = data$orders_rcvd
  
  cv.l2.fit = cv.glmnet(x,
                        y,
                        alpha = 1,
                        type.measure = error_type,
                        lambda = grid)
  
  best_lambda = cv.l2.fit$lambda.min
  #best_lambda
  best_lambda_index = match(best_lambda, cv.l2.fit$lambda)
  #best_lambda_index
  
  cv.l2.fit$cvm[best_lambda_index]
  
  l2.fit = glmnet(x, y, alpha = 1, lambda = best_lambda)
  coefs = coef(l2.fit)[, 1]
  coefs = coefs[coefs != 0]
  
  ret = list()
  for (var in names(coefs)) {
    #print(coefs[var])
    ret[var] = paste0(var, coefs[[var]], "<br>")
  }
  list(fit = cv.l2.fit, coefs = ret)
  
}