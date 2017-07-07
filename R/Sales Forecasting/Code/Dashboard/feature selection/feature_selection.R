library(glmnet)



## Globals
feature_selection_prefix = paste0(features_prefix, "featureSelection_")

sa_OR_nsa = "Not Seasonally Adjusted"



## UI Elements
initFeatureSelectionUI = function() {
  
  print("Feature Selection :: Feature Selection :: initFeatureSelectionUI() :: INIT")
  
  # row 1
  text_advanced_text = tags$h4("Advanced Options")
  
  row_1_col_width = 3
  row_1_textInput_width = '40%'
  text_lambda = column(row_1_col_width, tags$h5("lambda"))
  text_lambda_start = column(
    row_1_col_width,
    textInput(
      paste0(feature_selection_prefix, "lambda_start"),
      "lambda start",
      value = "0.001",
      width = row_1_textInput_width
    )
  )
  text_lambda_end = column(
    row_1_col_width,
    textInput(
      paste0(feature_selection_prefix, "lambda_end"),
      "lambda end value",
      value = "10",
      width = row_1_textInput_width
    )
  )
  text_lambda_lengths = column(
    row_1_col_width,
    textInput(
      paste0(feature_selection_prefix, "lambda_length"),
      "lengths",
      value = "100",
      width = row_1_textInput_width
    )
  )
  select_error_type =  column(row_1_col_width, selectInput(
    paste0(feature_selection_prefix, "selectErrorType"),
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
  button_run_LASSO = actionButton(inputId = paste0(feature_selection_prefix, "buttonLASSO"),
                                  label = "Run LASSO")
  row_2 = fluidRow(button_run_LASSO)
  
  # row 3
  output_graph = plotOutput(paste0(feature_selection_prefix, "outputLASSOGraph"))
  row_3 = fluidRow(output_graph)
  
  # row 4
  output_coefs = uiOutput(paste0(feature_selection_prefix, "outputLASSOSummary"))
  row_4 = fluidRow(output_coefs)
  
  # row 5
  button_select_them = actionButton(paste0(feature_selection_prefix, "buttonSelectThem"),
                                    label = "Select Them")
  row_5 = fluidRow(button_select_them)
  
  tabFeatureSelection = tabPanel(title = "Feature Selection",
                                 row_1,
                                 row_2,
                                 row_3,
                                 row_4,
                                 row_5)
  
  tabFeatureSelection
}



## Server Functions
readData = function(input, output) {
  print("Feature Selection :: Feature Selection :: readData() :: INIT")
  
  input_value = reactiveValuesToList(input)
  selected_vars = vector('character')
  for (key in names(input_value)) {
    #print(key)
    if (grepl("_fId",key) & input_value[[key]] == TRUE) {
      selected_vars = c(selected_vars, unlist(strsplit(key, "\\|"))[3])
    }
  }
  #print("All features that were selected")
  #print(selected_vars)
  
  config_data = meta_data[meta_data$series_id %in% selected_vars, ]
  
  # Y
  revenue_file = paste0(data_folder, "/", product_line, "/Revenue.csv")
  data_revenue = read.csv(revenue_file, header = TRUE, sep = ",")
  data = data_revenue[, c(product_data_column, "month", "t")]
  data$month = as.factor(data$month)
  
  # X
  for (sub_category_id in unique(config_data$sub_category_id)) {
    category_name = unique(config_data[config_data$sub_category_id == sub_category_id, "category_name"])
    sub_category_name = unique(config_data[config_data$sub_category_id ==
                                             sub_category_id, "sub_category_name"])
    
    
    file = paste0(
      data_folder, "/External Data/", as.character(category_name), "/",
      as.character(sub_category_name)
    )
    
    if (sa_OR_nsa == "Not Seasonally Adjusted") {
      file = paste0(file, "_nsa.csv")
    } else{
      file = paste0(file, "_sa.csv")
    }
    print(paste0("Reading file : ", file))
    data_series = read.csv(file, header = TRUE, sep = ",")
    
    series_vars = intersect(names(data_series), selected_vars)
    
    data = cbind(data, data_series[, series_vars])
    
  }
  
  print("Feature Selection :: Feature Selection :: readData() :: EXIT")
  filterFeatures(data, input, output)
  
}



filterFeatures = function(data, input, output) {
  print("Feature Selection :: Feature Selection :: filterFeatures() :: INIT")
  
  # Removing columns whose values don't change
  data = data[sapply(data, function(x)
    length(unique(x)) > 1)]
  
  # Removing highly correlated variables
  data_num_var = data[, !names(data) %in% c("month", product_data_column, "t")]
  tmp = cor(data_num_var)
  tmp[!lower.tri(tmp)] = 0
  uncorrelated_vars = names(data_num_var[, !apply(tmp, 2, function(x)
    any(x > 0.99))])
  #print("UnCorrelated Vars")
  #print(uncorrelated_vars)
  
  data.new = data[, c(product_data_column, "month", "t", uncorrelated_vars)]
  
  #print("Feature Selection :: Feature Selection :: filterFeatures() :: EXIT")
  #print(names(data.new))
  doLASSO(data.new, input, output)
  
}



doLASSO = function(data, input, output) {
  
  print("Feature Selection :: Feature Selection :: doLASSO() :: INIT")
  print(paste0("Product_Line=", product_line, ", Product_Column=", product_data_column))
  
  lambda_start = as.numeric(input[[paste0(feature_selection_prefix, "lambda_start")]])
  lambda_end = as.numeric(input[[paste0(feature_selection_prefix, "lambda_end")]])
  lambda_length = as.integer(input[[paste0(feature_selection_prefix, "lambda_length")]])
  error_type = as.character(input[[paste0(feature_selection_prefix, "selectErrorType")]])
  
  grid = 2.71828 ^ seq(lambda_start, lambda_end, length = lambda_length)
  
  form = as.formula(paste0(product_data_column," ~ ."))
  x = model.matrix(form, data)[, -1]
  y = data[,product_data_column]
  
  cv.l2.fit = cv.glmnet(x,
                        y,
                        alpha = 1,
                        type.measure = error_type,
                        lambda = grid)
  
  best_lambda = cv.l2.fit$lambda.min
  #best_lambda
  best_lambda_index = match(best_lambda, cv.l2.fit$lambda)
  #best_lambda_index
  
  best_error = cv.l2.fit$cvm[best_lambda_index]
  
  l2.fit = glmnet(x, y, alpha = 1, lambda = best_lambda)
  coefs = coef(l2.fit)[, 1]
  coefs = coefs[coefs != 0]
  
  
  error_id = paste0(feature_selection_prefix, "LASSOSummaryError")
  text_error = textInput(error_id, label = error_type, value = best_error)
  div_error = tags$div(text_error, style="float:left;")
  lambda_id = paste0(feature_selection_prefix, "LASSOSummarylambda")
  text_lambda = textInput(lambda_id, label = "Best log(Lambda)", value = log(best_lambda))
  div_lambda =   tags$div(text_lambda, style="float:left;")
  row_summary = fluidRow(div_error, div_lambda)
  
  corr_id = paste0(feature_selection_prefix, "LASSOSummaryVarCorr")
  output_var_corr = textOutput(corr_id)
  plot_id = paste0(feature_selection_prefix, "LASSOSummaryVarPlot")
  output_var_plot = plotOutput(plot_id)
  var_summary = fluidRow(output_var_plot, output_var_corr)
  
  selected_vars = lapply(names(coefs), function(var){ 
    
    fId = paste0(feature_selection_prefix, "fid|", var)
    var_name = meta_data[meta_data$series_id==var,"title"]
    
    #print(var)
    if(grepl("Intercept",var) | grepl("month",var) | var=="t"){
      text_var = tags$div(title=var_name, 
                          textInput(fId, label=var, value=coefs[[var]]))
    }else{
      button_id = paste0(fId, "|infoButton")
      input_button_var_info = actionButton(button_id, label="",
                                           icon("area-chart",lib="font-awesome"))
      
      observeEvent(input[[button_id]],{
        series = getSeries(var)
        output[[plot_id]] = renderPlot({
          plot(y,series)
        })
        output[[corr_id]] = renderText({
          print(paste0("Correlation with output var : ", cor(series, y)))
        })
      })
      
      text_var_value = textInput(fId, label=var_name, value=coefs[[var]])
      text_var = tags$div(title=var, 
                          tags$div(text_var_value, style="float:left;"), 
                          tags$div(input_button_var_info))
    }
    
    row_var = fluidRow(text_var)
    row_var
    
  })   
  
  row_selected_vars = fluidRow(column(width=6, selected_vars),
                               column(width=6, var_summary))
  
  LASSO_summary = fluidRow(row_summary, tags$hr(), row_selected_vars)
  
  list(fit = cv.l2.fit, coefs_ui = LASSO_summary, coefs_names=names(coefs))
  
}



attachObservers = function(input, output, session, reactive_vars){
  
  print("Feature Selection :: Feature Selection :: attachObservers() :: INIT")
  
  # Perform LASSO Regression
  featureSelection_LASSO = paste0(feature_selection_prefix, "buttonLASSO")
  observeEvent(input[[featureSelection_LASSO]], {
    
    # Function Flow : readData > filterFeatures > doLASSO
    fit_and_coefs = readData(input, output)
    
    output_LASSOgraph = paste0(feature_selection_prefix, "outputLASSOGraph")
    output[[output_LASSOgraph]] = renderPlot({
      plot(fit_and_coefs[["fit"]])
    })
    
    output_LASSOcoefs = paste0(feature_selection_prefix, "outputLASSOSummary")
    output[[output_LASSOcoefs]] = renderUI({
      fit_and_coefs["coefs_ui"]
    })
    
    reactive_vars[['selected_vars']] = fit_and_coefs[['coefs_names']]
    
  })
  
  
  # Select short-listed features for Model and Forecast Tab
  button_select_them = paste0(feature_selection_prefix, "buttonSelectThem")
  observeEvent(input[[button_select_them]], {
    
    table_regression_varaibles = paste0(regression_prefix, "selectedVariables")
    output[[table_regression_varaibles]] = renderUI({
      createVariableTable(reactive_vars[['selected_vars']])
    })
    
    table_forecast_varaibles = paste0(forecast_prefix, "forecastVariables")
    output[[table_forecast_varaibles]] = renderUI({
      createForecastVariableTable(reactive_vars[['selected_vars']], input, output, session)
    })
    
  })
  
}