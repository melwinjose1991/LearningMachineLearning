## Globals
forecast_prefix = paste0(forecast_tab_prefix, "forecast_")



## UI Elements
getForecastUI = function(){
  
  # row 1 - variables
  text_variables = tags$h4("Variables")
  input_vars = uiOutput(paste0(forecast_prefix, "forecastVariables"))
  row_1 = fluidRow(text_variables, input_vars)
  
  # row 2 - Buttons
  button_forecast_id = paste0(forecast_prefix, "buttonForecast")
  button_forecast = actionButton(inputId = button_forecast_id,
                                         label = "Forecast")
  row_2 = fluidRow(button_forecast)
  
  # row 3 - plots
  id = paste0(forecast_prefix, "graphForeCast")
  output_graph_forecast = plotOutput(id)
  row_3 = fluidRow(output_graph_forecast)
  
  # row 4 - ???
  row_4 = fluidRow()
  
  # 
  tabPanel(title = "Forecast", row_1, row_2, row_3, row_4)
  
}



## Server Functions
createForecastVariableTable = function(variables, input, output, session){
  
  print(paste0("forecast :: createForecastVariableTable :: START"))
  
  column_width_var_name = 1
  column_width_graph = 6
  column_width_var_value = 2
  column_width_method = 2
  column_width_params = 1
    
  vars_titles = list(
    fluidRow(
      column(width=column_width_var_name, tags$h5("Variable")),
      column(width=column_width_graph, tags$h5("Graph")),
      column(width=column_width_var_value, tags$h5("Value")),
      column(width=column_width_method, tags$h5("Method")),
      column(width=column_width_params, tags$h5("Params"))
    )
  )
  
  variables = variables[!grepl("(Intercept)",variables)]
  vars_rows = lapply(variables, function(var){
    
    is_var_numerical = ifelse(grepl("month", var), FALSE, TRUE)
    
    var_id = paste0(forecast_prefix, "varId|", var)
    var_name = meta_data[meta_data$series_id==var,"title"]
    output_var_name = tags$div(title=var_name, tags$h5(var))
    
    if(is_var_numerical){
      
      # Value to use
      var_value_id = paste0(var_id, "|value")
      input_text_var_value = textInput(var_value_id, label=NULL, placeholder="Enter or Select")
      
      # Graph
      output_var_graph_id = paste0(var_id, "|graph")
      output_var_graph = plotOutput(output_var_graph_id)
      output[[output_var_graph_id]] = renderPlot({
        #print(getSeries(var))
        plot(getSeries(var), ylab=var)
      })
        
      # Method
      var_method_id = paste0(var_id, "|method")
      input_select_method = selectInput(var_method_id, label=NULL, 
                                    c("User Entered" = "user",
                                      "Use Mean" = "mean",
                                      "Use Last Value" = "last_value",
                                      "Time-Series x" = "time_series"
                                    ))
  
      observeEvent(input[[var_method_id]], {
        variable_id = unlist(strsplit(var_method_id,"\\|"))[2]
        var_series = getSeries(variable_id)
        print(paste0("forecast :: createForecastVariableTable :: triggered var ",variable_id))      
        
        method = input[[var_method_id]]
        print(paste0("forecast :: createForecastVariableTable :: method=",method))
        if(method=="user"){
          value = input[[var_value_id]]
        }else if(method=="mean"){
          value = mean(var_series)
        }else if(method=="last_value"){
          value = tail(var_series, n=1)
        }else{
          value = 0
        }
        print(paste0("forecast :: createForecastVariableTable :: value=",value))
        
        var_value_id = paste0(forecast_prefix, "varId|", variable_id, "|value")
        updateTextInput(session, var_value_id, value=value)
      })
      
      # Method Params
      var_method_params_id = paste0(var_id, "|parameters")
      input_method_params = tags$div(id=var_method_params_id, "params")
      
      # Rows
      fluidRow(
        column(width=column_width_var_name, output_var_name),
        column(width=column_width_graph, output_var_graph),
        column(width=column_width_var_value, input_text_var_value),
        column(width=column_width_method, input_select_method),
        column(width=column_width_params, input_method_params)
      )
      
    }else{
      
      # Value to use
      # NOTE : need to generalized to categorical variables with 
      #        more number of values
      var_value_id = paste0(var_id, "|value")
      input_select_var_value = checkboxInput(var_value_id, label=NULL)
      
      fluidRow(
        column(width=column_width_var_name, output_var_name),
        column(width=column_width_graph),
        column(width=column_width_var_value, input_select_var_value)
      )
      
    }
    
  })
  
  print(paste0("forecast :: createForecastVariableTable :: END"))
  list(vars_titles, vars_rows)
  
}



getSeries = function(var_id){
  
  print(paste0("forecast :: getSeries :: START"))
  print(paste0("forecast :: getSeries :: params(",var_id,")"))
    
  config_data = meta_data[meta_data$series_id %in% var_id, ]
  
  # X
  if(length(unique(config_data$sub_category_id)) == 1) {
    sub_category_id = unique(config_data$sub_category_id)
    
    category_name = unique(config_data[config_data$sub_category_id==sub_category_id, "category_name"])
    sub_category_name = unique(config_data[config_data$sub_category_id==sub_category_id, "sub_category_name"])
    
    file = paste0(
      data_folder,product,"/",as.character(category_name),
      "/",as.character(sub_category_name)
    )
    
    if (sa_OR_nsa == "Not Seasonally Adjusted") {
      file = paste0(file, "_nsa.csv")
    } else{
      file = paste0(file, "_sa.csv")
    }
    print(paste0("Reading file : ", file))
    series_df = read.csv(file, header = TRUE, sep = ",")
    
    series_df[,var_id]
    
  }else{
    print(paste0("forecast :: getSeries :: requested series ", var_id," doesn't exists OR duplicates exists"))
  }
  
}

getForecastResults=function(y_name="orders_rcvd", forecast_model, input_variables, h=1, 
                             mean_model=TRUE, naive_model=TRUE, snaive_model=TRUE,
                             drift_model=TRUE){
  
  data = read.csv(revenue_file, header = TRUE, sep = ",")
  y = data[,y_name]

  t = ts(c(y, rep(NA,h)), frequency=12)
  plot(t)
  results = list(t=t)
  t_window = window(t, end=4.999)
  t_window
  
  ## Average Method
  if(mean_model){
    predictions = meanf(t_window, h)$mean
    lines(predictions, col=2, lwd=2, lty=2)
    
    results[['line_mean']] = predictions
  }
  
  ## Naive Method
  if(naive_model){
    predictions = naive(t_window, h)$mean
    lines(predictions, col=3, lwd=2, lty=2)

    results[['line_naive']] = predictions
  }
  
  ## Seasonal Naive Method
  if(snaive_model){
    predictions = snaive(t_window, h)$mean
    lines(predictions, col=4, lwd=2, lty=2)
    
    results[['line_snaive']] = predictions
  }
  
  ## Drift Method
  if(drift_model){
    predictions = rwf(t_window, h, drift=TRUE)$mean
    lines(predictions, col=5, lwd=2, lty=2)
    
    results[['line_drift']] = predictions
  }
  
  # Predictions by your model
  predictions = predict(forecast_model, newdata=input_variables)
  predictions = c(tail(y,n=1), predictions)
  print(predictions)
  predictions = ts(predictions, frequency = 12, start=5-0.0833)
  lines(predictions, col=6, lwd=2, lty=2)
  
  results[['line_modelX']] = predictions
  
  
  legend("topleft", lwd=2, lty=2, col=c(2,3,4,5,9),
         legend=c("Mean","Naive","Seasonal Naive","Drift","ModelX"))
  
  results
  
}