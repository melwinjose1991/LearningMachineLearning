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
  
  text_lambda = column(
    row_1_col_width,
    textInput(
      paste0(feature_selection_prefix, "lambda_start"),
      "lambda start",
      value = "0.001",
      width = row_1_textInput_width
    ),
    textInput(
      paste0(feature_selection_prefix, "lambda_end"),
      "lambda end",
      value = "10",
      width = row_1_textInput_width
    ),
    textInput(
      paste0(feature_selection_prefix, "lambda_length"),
      "lengths",
      value = "100",
      width = row_1_textInput_width
    )
  )
  
  text_kfolds_alpha = column(
    row_1_col_width,
    textInput(
      paste0(feature_selection_prefix, "kFolds"),
      "k-folds",
      value = "24",
      width = row_1_textInput_width
    ),
    textInput(
      paste0(feature_selection_prefix, "alpha"),
      "alpha",
      value = "1",
      width = row_1_textInput_width
    )
  )
  
  column_select_error = selectInput(
    paste0(feature_selection_prefix, "selectErrorType"),
    "Error Type:",
    c(
      "Mean Square Error" = "mse",
      "Mean Absolute Error" = "mae"
    ),
    selected="mae"
  )
  
  column_select_filter = selectInput(
    paste0(feature_selection_prefix, "selectFilter"),
    "Filter:",
    c(
      "Direct:Correlation" = "direct_corr_filter",
      "CARET:Correlation" = "caret_corr_filter"
    ),
    selected="direct_corr"
  )
  
  column_selects =  column(row_1_col_width, column_select_error, 
                           column_select_filter)
  
  row_1 = fluidRow(
    text_advanced_text,
    text_lambda,
    text_kfolds_alpha,
    column_selects
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
  # button_select_them = actionButton(paste0(feature_selection_prefix, "buttonSelectThem"),
  #                                  label = "Select Them")
  # row_5 = fluidRow(button_select_them)
  
  tabFeatureSelection = tabPanel(title = "Feature Selection",
                                 row_1,
                                 row_2,
                                 row_3,
                                 row_4)
  
  tabFeatureSelection
}



## Server Functions
readData = function(input, output, session) {
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
    
    
    file = paste0( FRED_folder, "/", as.character(category_name), 
                  "/", as.character(sub_category_name) )
    
    if (sa_OR_nsa == "Not Seasonally Adjusted") {
      file = paste0(file, "_nsa.csv")
    } else{
      file = paste0(file, "_sa.csv")
    }
    print(paste0("Reading file : ", file))
    data_series = read.csv(file, header = TRUE, sep = ",")
    
    #series_vars = intersect(names(data_series), selected_vars)
    #data = cbind(data, data_series[, series_vars])

    series_vars = intersect(names(data_series), selected_vars)
    if(is.null(dim(data_series[, series_vars]))){
      ## there is just one column from the series
      data[,series_vars] = data_series[, series_vars]
    }else{
      data = cbind(data, data_series[, series_vars])  
    }
    
  }
  
  print("Feature Selection :: Feature Selection :: readData() :: EXIT")
  filterFeatures(data, input, output, session)
  
}



filterFeatures = function(data, input, output, session) {
  print("Feature Selection :: Feature Selection :: filterFeatures() :: INIT")
  
  # Removing columns whose values don't change
  data.new = removeUnchangingVariables(data)
  
  # Filter
  data.new = switch(input[[paste0(feature_selection_prefix, "selectFilter")]],
                    direct_corr_filter = removeCorrelatedVariables(data.new),
                    caret_corr_filter =  removeCorrelatedVariablesCARET(data.new)
  )
  
  #print("Feature Selection :: Feature Selection :: filterFeatures() :: EXIT")
  #print(names(data.new))
  doLASSO(data.new, input, output, session)
  
}



doLASSO = function(data, input, output, session) {
  
  print("Feature Selection :: Feature Selection :: doLASSO() :: INIT")
  print(paste0("Product_Line=", product_line, ", Product_Column=", product_data_column))
  
  
  # parameters for LASSO
  lambda_start = as.numeric(input[[paste0(feature_selection_prefix, "lambda_start")]])
  lambda_end = as.numeric(input[[paste0(feature_selection_prefix, "lambda_end")]])
  lambda_length = as.integer(input[[paste0(feature_selection_prefix, "lambda_length")]])
  error_type = as.character(input[[paste0(feature_selection_prefix, "selectErrorType")]])
  nfolds = as.numeric(input[[paste0(feature_selection_prefix, "kFolds")]])
  alpha = as.numeric(input[[paste0(feature_selection_prefix, "alpha")]])
  grid = 2.71828 ^ seq(lambda_start, lambda_end, length = lambda_length)
  
  obs_to_exclude = input[[paste0(products_prefix, product, "|avoid")]]
  obs_to_exclude = as.numeric(unlist(strsplit(obs_to_exclude,",")))
  weights = rep(1, nrow(data))
  weights[obs_to_exclude] = 0
  
  # Varaibles and Predictors
  form = as.formula(paste0(product_data_column," ~ ."))
  x = model.matrix(form, data)[, -1]
  y = data[,product_data_column]
  
  
  # LASSO
  cv.l2.fit = cv.glmnet(x, y, type.measure = error_type,
                        alpha = alpha, nfolds=nfolds,
                        lambda = grid, weights=weights)
  
  getLASSOModels(data, input, output, session, cv.l2.fit)
}  



getLASSOModels = function(data, input, output, session, cv.l2.fit){
  
  form = as.formula(paste0(product_data_column," ~ ."))
  x = model.matrix(form, data)[, -1]
  y = data[,product_data_column]
  
  # best model
  best_lambda = cv.l2.fit$lambda.min
  best_lambda_index = match(best_lambda, cv.l2.fit$lambda)
  best_error = cv.l2.fit$cvm[best_lambda_index]
  best_fit = glmnet(x, y, alpha = 1, lambda = best_lambda)
  best_coefs = coef(best_fit)[, 1]
  best_coefs = best_coefs[best_coefs != 0]
  print(best_coefs)
  
  # simpler model - 1SE
  simple_lambda = cv.l2.fit$lambda.1se
  simple_lambda_index = match(simple_lambda, simple_lambda)
  simple_error = cv.l2.fit$cvm[simple_lambda_index]
  simple_fit = glmnet(x, y, alpha = 1, lambda = simple_lambda)
  simple_coefs = coef(simple_fit)[, 1]
  simple_coefs = simple_coefs[simple_coefs != 0]
  print(simple_coefs)
  
  best_summary = getModelSummary(data, input, output, session, "best_",
                                 best_coefs, best_error, best_lambda)
  simple_summary = getModelSummary(data, input, output, session, "simple_",
                                   simple_coefs, simple_error, simple_lambda)
  
  list(fit=cv.l2.fit, best_coefs_ui=best_summary[['coefs_ui']], best_coefs_names=names(best_coefs),
       simple_coefs_ui=simple_summary[['coefs_ui']], simple_coefs_names=names(simple_coefs))
}



getModelSummary = function(data, input, output, session, model_name, coefs, error, lambda){
  
  y = data[,product_data_column]
  model_id = paste0(feature_selection_prefix, model_name)
  error_type = as.character(input[[paste0(feature_selection_prefix, "selectErrorType")]])  
  
  # Error and Lambda
  error_id = paste0(model_id, "LASSOSummaryError")
  text_error = textInput(error_id, label = error_type, value = error)
  div_error = tags$div(text_error, style="float:left;")
  
  lambda_id = paste0(model_id, "LASSOSummarylambda")
  text_lambda = textInput(lambda_id, label = "Best log(Lambda)", value = log(lambda))
  div_lambda =   tags$div(text_lambda, style="float:left;")
  
  row_summary = fluidRow(div_error, div_lambda)
  
  
  # Correlation and Plot
  corr_id = paste0(model_id, "LASSOSummaryVarCorr")
  output_var_corr = htmlOutput(corr_id)
  
  plot_id = paste0(model_id, "LASSOSummaryVarPlot")
  output_var_plot = plotOutput(plot_id)
  
  var_summary = fluidRow(output_var_plot, output_var_corr)
  
  
  # UI Element for each var
  selected_vars = lapply(names(coefs), function(var){ 
    
    fId = paste0(model_id, "fid|", var)
    var_name = meta_data[meta_data$series_id==var,"title"]
    
    if(grepl("Intercept",var) | grepl("month",var) | var=="t"){
      text_var = tags$div(title=var_name, 
                          textInput(fId, label=var, value=coefs[[var]]))
    }else{
      
      button_id = paste0(fId, "|infoButton")
      input_button_var_info = actionButton(button_id, label="",
                                           icon("area-chart",lib="font-awesome"))
      observeEvent(input[[button_id]],{
        series = getSeries(var)
        my_df = data.frame(xx=series,yy=y)
        fit = lm(yy~xx, data=my_df)
        
        output[[plot_id]] = renderPlot({
          plot(series, y, ylab=paste0(product_line, " - ", product_data_column), 
               xlab=var_name)
          abline(fit)
        })
        
        output[[corr_id]] = renderUI({
          res_corr = paste0("Correlation : ", cor(series, y))
          res_slope = paste0("Slope : ", fit$coefficients[2])
          HTML(paste(res_corr, res_slope, sep="<br/>"))
        })
      })
      
      text_var_value = textInput(fId, label=var_name, value=coefs[[var]])
      
      select_var_id = paste0(fId,"|select")
      input_select_var = checkboxInput(select_var_id, label="", value=TRUE)
      observeEvent(input[[select_var_id]],{
        group_id = meta_data[meta_data$series_id==var, "sub_category_id"]
        #id = paste0(all_features_prefix, "fId|", group, "|", var)
         id = paste0(all_features_prefix, "fId|", group_id, "|", var)
        if(input[[select_var_id]]){
          updateTextInput(session, id, value=TRUE)
        }else{
          updateTextInput(session, id, value=FALSE)
        }
      })
            
      text_var = tags$div(title=var, 
                          tags$div(text_var_value, style="float:left;"), 
                          tags$div(input_button_var_info),
                          tags$div(input_select_var))
    }
    
    row_var = fluidRow(text_var)
    row_var
    
  })   
  
  row_selected_vars = fluidRow(column(width=6, selected_vars),
                               column(width=6, var_summary))
  
  button_id = paste0(model_id, "buttonSelectThem")
  button_select_them = actionButton(button_id, label = "Select Model")
  
  LASSO_summary = fluidRow(row_summary, tags$hr(), row_selected_vars, button_select_them)
  
  list(coefs_ui = LASSO_summary)
  
}



attachObservers = function(input, output, session, reactive_vars){
  
  print("Feature Selection :: Feature Selection :: attachObservers() :: INIT")
  
  # Perform LASSO Regression
  featureSelection_LASSO = paste0(feature_selection_prefix, "buttonLASSO")
  observeEvent(input[[featureSelection_LASSO]], {
    
    # Function Flow : readData > filterFeatures > doLASSO
    fit_and_coefs = readData(input, output, session)
    
    output_LASSOgraph = paste0(feature_selection_prefix, "outputLASSOGraph")
    output[[output_LASSOgraph]] = renderPlot({
      plot(fit_and_coefs[["fit"]])
    })
    
    output_LASSOcoefs = paste0(feature_selection_prefix, "outputLASSOSummary")
    output[[output_LASSOcoefs]] = renderUI({
      best_tab = tabPanel("Best Model", fit_and_coefs["best_coefs_ui"])
      simple_tab = tabPanel("Simple Model", fit_and_coefs["simple_coefs_ui"])
      tab_menu = tabsetPanel(best_tab, simple_tab)
      
      list(menu=tab_menu)
    })
    
    reactive_vars[['best_selected_vars']] = fit_and_coefs[['best_coefs_names']]
    reactive_vars[['simple_selected_vars']] = fit_and_coefs[['simple_coefs_names']]
    
  })
  
  
  # Select short-listed features for Model and Forecast Tab
  attachSelectButtonObservers("best_", input, output, session, reactive_vars)
  attachSelectButtonObservers("simple_", input, output, session, reactive_vars)

}

attachSelectButtonObservers = function(model_name, input, output, session, reactive_vars){
  
  button_id = paste0(feature_selection_prefix, model_name, "buttonSelectThem")
  observeEvent(input[[button_id]], {
    
    selected_vars_id = paste0(model_name,"selected_vars")
    
    table_regression_varaibles = paste0(regression_prefix, "selectedVariables")
    output[[table_regression_varaibles]] = renderUI({
      createVariableTable(reactive_vars[[selected_vars_id]], input, session)
    })
    
    table_forecast_varaibles = paste0(forecast_prefix, "forecastVariables")
    output[[table_forecast_varaibles]] = renderUI({
      createForecastVariableTable(reactive_vars[[selected_vars_id]], input, output, session)
    })
    
    reactive_vars[['selected_vars']] = reactive_vars[[selected_vars_id]]
  })
  
}