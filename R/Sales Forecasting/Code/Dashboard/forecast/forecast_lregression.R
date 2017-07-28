print("Forecast :: forecast_lregression.R :: Init")



## Globals
forecast_prefix = paste0(forecast_tab_prefix, "forecast_")



## UI Elements
getForecastLRegressionUI = function(){
  
  # row 0 - ARIMA options
  id = paste0(forecast_prefix, "fastARIMA")
  checkbox_fast_arima = checkboxInput(id, "FAST-ARIMA", TRUE)
  row_0 = fluidRow(column(6, checkbox_fast_arima))
  
  # row 1 - variables
  text_variables = tags$h4("Variables")
  input_vars = uiOutput(paste0(forecast_prefix, "forecastVariables"))
  column_var_name = column(width=12, text_variables, input_vars)
  row_1 = fluidRow(column_var_name)
  
  # row 2 - Buttons
  button_forecast_id = paste0(forecast_prefix, "buttonForecast")
  button_forecast = actionButton(inputId = button_forecast_id,
                                         label = "Forecast")
  row_2 = fluidRow(button_forecast)
  
  # row 3 - plots
  id = paste0(forecast_prefix, "forecastPlot")
  column_graph_forecast = column(width=8,plotOutput(id))
  id = paste0(forecast_prefix, "forecastValue")
  column_value_forecast = column(width=4, htmlOutput(id))
  row_3 = fluidRow(column_graph_forecast, column_value_forecast)
  
  # row 4 - ???
  row_4 = fluidRow()
  
  tabPanel(title = "LRegression", row_0, row_1, tags$hr(), row_2, row_3, row_4)
  
}



## Server Functions
createForecastVariableTable = function(variables, input, output, session){
  
  print(paste0("forecast :: createForecastVariableTable :: START"))
  
  column_width_var_name = 5
  column_width_var_value = 1
    
  cols_forecast_periods_header = lapply(1:no_of_forecast, function(i){
    tag_name = paste0("month-",i)
    column(width=column_width_var_value, tags$h5(tag_name))
  })
  
  vars_titles = list(
    fluidRow(
      cols_forecast_periods_header
    )
  )
  
  variables = variables[!grepl("(Intercept)",variables)]
  vars_rows = lapply(variables, function(var){
    
    is_var_numerical = ifelse(grepl("month", var), FALSE, TRUE)
    
    var_id = paste0(forecast_prefix, "varId|", var)
    var_name = meta_data[meta_data$series_id==var,"title"]
    if(length(var_name)==0){
      var_name = var
    }
    output_var_name = tags$h5(var_name)
    
    if(is_var_numerical){
      
      # Forecaste Button
      input_button_var_forecast_id = paste0(var_id, "|infoButton")
      input_button_var_forecast = actionButton(input_button_var_forecast_id, label="",
                                           icon("area-chart",lib="font-awesome"))
      observeEvent(input[[input_button_var_forecast_id]],{
        
        var_name_id = paste0(forecast_prefix,"varName")
        output[[var_name_id]] = renderUI({ list(tags$h5(var)) })
        
        series = getSeries(var)
        series_ts = ts(series, frequency=12, 
                     start=product_start_date, end=product_end_date)
        fast_arima = input[[paste0(forecast_prefix, "fastARIMA")]]
        var_forecast = doAutoARIMA(series_ts, no_of_forecast, 
                                   stepwise=fast_arima, approximation=fast_arima)
        
        # plotting graph
        graph_id = paste0(var_id, "|varInfoGraph") 
        output[[graph_id]] = renderPlot(
          plot(var_forecast[['forecast_fit']])
        )
        
        # populating summary
        summary_id = paste0(forecast_prefix, "varInfoSummary") 
        summ = summary(series)
        summ_cols = names(summ)
        output[[summary_id]] = renderText(
          paste0(summ_cols,"=",summ," | ")
        )
        
        # populating values
        lapply(1:no_of_forecast, function(i){
          var_value_id = paste0(var_id, "|value|",i)
          value = var_forecast[['forecast_fit']]$mean[i]
          updateTextInput(session, var_value_id, value=value)
        })
        
      })
      
      # Variable Row - 1 : Name and Buttons
      var_name_info_row = fluidRow(column(column_width_var_name, output_var_name), 
                                   column(1,input_button_var_forecast))
      
      # Variable Row - 2 : Value for periods
      cols_forecast_periods_values = lapply(1:no_of_forecast, function(i){
        var_value_id = paste0(var_id, "|value|",i)
        input_text_var_value = textInput(var_value_id, label=NULL)
        column(width=column_width_var_value, input_text_var_value)
      })
      
      var_value_row = fluidRow( column(no_of_forecast, cols_forecast_periods_values) )
      
      # Variable Row - 3 : Graphs and Summary
      graph_id = paste0(var_id, "|varInfoGraph") 
      var_graph = plotOutput(graph_id)
      var_plot_summary = fluidRow(var_graph)
      
      # Rows
      fluidRow( var_name_info_row, var_value_row, var_plot_summary, tags$hr() )
      
    }else{
      
      # Value to use
      # NOTE : need to generalized to categorical variables with 
      #        more number of values
      cols_forecast_periods_values = lapply(1:no_of_forecast, function(i){
        var_value_id = paste0(var_id, "|value|",i)
        input_select_var_value = checkboxInput(var_value_id, label=NULL)
        column(width=column_width_var_value, input_select_var_value)
      })
      
      var_value_row = fluidRow( column(no_of_forecast, cols_forecast_periods_values) )
      
      fluidRow(
        column(width=column_width_var_name, output_var_name),
        var_value_row,
        tags$hr()
      )
      
    }
    
  })
  
  print(paste0("forecast :: createForecastVariableTable :: END"))
  list(vars_titles, tags$hr(), vars_rows)
  
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
    
    file = paste0(FRED_folder, "/", as.character(category_name), 
                  "/", as.character(sub_category_name) )
    
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
  
  revenue_file = paste0(data_folder, "/", product_line, "/Revenue.csv")
  data = read.csv(revenue_file, header = TRUE, sep = ",")
  y = data[,y_name]

  t_window = ts(c(y, rep(NA,h)), frequency=12, start=product_start_date)
  plot(t_window)
  results = list(t=t_window)
  
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
  last_ym = as.yearmon(paste0(product_end_date,collapse="-"))
  forecast_start_ym = format(last_ym + (1/12),"%Y-%m")
  forecast_start_ym = as.numeric(unlist(strsplit(forecast_start_ym,"-")))
  
  pred = predict(forecast_model, newdata=input_variables, interval="predict")
  
  predictions = pred[,1]
  predictions = ts(predictions, frequency = 12, start=forecast_start_ym)
  lines(predictions, col=6, lwd=2, lty=2)
  print(predictions)
  
  predictions_lwr = ts(pred[,2], frequency=12, start=forecast_start_ym)
  lines(predictions_lwr, col=6, lwd=2, lty=2)
  print(predictions_lwr)
  
  predictions_upr = ts(pred[,3], frequency=12, start=forecast_start_ym)
  lines(predictions_upr, col=6, lwd=2, lty=2)
  print(predictions_upr)
  
  
  results[['line_modelX']] = predictions
  results[['line_modelX_upr']] = predictions_upr
  results[['line_modelX_lwr']] = predictions_lwr
  
  legend("topleft", lwd=2, lty=2, col=c(2,3,4,5,9),
         legend=c("Mean","Naive","Seasonal Naive","Drift","ModelX"))
  
  results
  
}



attachForecastObservers = function(input, output, reactive_vars){
  
  forecast_do_forecast = paste0(forecast_prefix, "buttonForecast")
  observeEvent(input[[forecast_do_forecast]], {
    print(">>> Forecasting <<<")
    
    h = no_of_forecast # Has to be user entered
    mean_model = FALSE #input[[paste0(forecast_prefix,"methodId|mean")]]
    naive_model = FALSE #input[[paste0(forecast_prefix,"methodId|naive")]]
    snaive_model = FALSE #input[[paste0(forecast_prefix,"methodId|snaive")]]
    drift_model = FALSE #input[[paste0(forecast_prefix,"methodId|drift")]]
    
    forecast_model = reactive_vars[[MODEL_LINEAR_REGRESSION]]
    for(var in reactive_vars[[MODEL_LINEAR_REGRESSION_VARS]]){
      if(!exists("variable_values")){
        variable_values = data.frame(row=1:h)
      }
      variable_values[,var] = unlist(lapply(1:h, function(i){
        var_id = paste0(forecast_prefix, "varId|", var, "|value|", i)
        if(is.logical(input[[var_id]])){
          ifelse(input[[var_id]],1,0)
        }else{
          as.numeric(input[[var_id]])
        }
      }))
    }
    
    print(variable_values)
    
    last_ym = as.yearmon(paste0(product_end_date,collapse="-"))
    forecast_start_ym = format(last_ym + (1/12),"%Y-%m")
    forecast_start_ym = as.numeric(unlist(strsplit(forecast_start_ym,"-")))
    results = getForecastResults(forecast_model=forecast_model, 
                                 input_variables=variable_values, 
                                 h=h, mean_model=mean_model, naive_model=naive_model,
                                 snaive_model=snaive_model, drift_model=drift_model)
    
    ## Converting to data_frame for iterability
    f_interval = abs(results[['line_modelX_upr']] - results[['line_modelX_lwr']])
    df = data.frame(n=1:no_of_forecast)
    df[,DF_COL_LREG_FORECAST] = results[['line_modelX']]
    df[,DF_COL_LREG_LWR] = results[['line_modelX_lwr']]
    df[,DF_COL_LREG_UPR] = results[['line_modelX_upr']]
    df[,DF_COL_LREG_INTERVAL] = f_interval
    
    ## Saving forecasts for ensembling
    reactive_vars[[FORECAST_LREGRESSION]] = df
    
    
    ## Plotting graphs
    output_graph_forecast = paste0(forecast_prefix, "forecastPlot")
    output[[output_graph_forecast]] = renderPlot({
      
      t = ts(results[["t"]], frequency=12, start=product_start_date)
      plot(t, xlab=product_data_column, ylab="Time")
      
      models = vector('character')
      colors = c(2,3,4,5,9)
      index = 1
      for(result in names(results)){
        
        if(grepl("line_", result)){
          if( grepl("upr",result) | grepl("lwr",result) ){
            t = ts(results[[result]], frequency=12, start=forecast_start_ym)
            lines(t, col="blue", lty=2)
          }else{
            t = ts(results[[result]], frequency=12, start=forecast_start_ym)
            lines(t, col=colors[index], lwd=2, lty=2)
            
            model = unlist(strsplit(result,"_"))[2]
            models = c(models, model)
            
            index = index + 1
          }
        }
        
      }
      
      legend("topleft", lwd=2, lty=2, col=colors, legend=models)
      
    })
    
    ## Forecast values
    output_value_forecast = paste0(forecast_prefix, "forecastValue")
    output[[output_value_forecast]] = renderUI({
      
      text_forecast = sapply(1:nrow(df), function(i){
        row = df[i,]
        fit = round(row[[DF_COL_LREG_FORECAST]], 2)
        lwr = round(row[[DF_COL_LREG_LWR]], 2)
        upr = round(row[[DF_COL_LREG_UPR]], 2)
        interval = round(row[[DF_COL_LREG_INTERVAL]], 2)
        
        text = paste0("<tr><td>&nbsp;", i ,"&nbsp</td>",
                      "<td>&nbsp;", fit ,"&nbsp</td>",
                      "<td>&nbsp;",lwr,"&nbsp;</td>",
                      "<td>&nbsp;",upr, "&nbsp;</td>",
                      "<td>&nbsp;", interval,"&nbsp;</td></tr>")
        text
      })
      text_forecast = paste0(text_forecast, collapse="")
      text_forecast = paste0("<table><tr><th>#</th>",
                             "<th>Forecast</th>",
                             "<th>Lower</th><th>Upper</th>",
                             "<th>Interval</th></tr>",
                             text_forecast,"</table>")
      
      
      HTML(text_forecast)
      
    })
    
  })
  
}