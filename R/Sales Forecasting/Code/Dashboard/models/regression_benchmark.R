print("Models :: Benchmark :: Init")

library(forecast)



## Globals
benchmark_prefix = paste0(models_prefix, "regressionBenchmark_")



## UI Elements
getRegressionBenchmarkUI = function(){
  
  # row_1 - options
  id = paste0(benchmark_prefix, "h")
  input_numeric_h = numericInput(id, "Forecast Period", value=12, min=2, max=12, 
                                 step=1, width="50%")
  
  id = paste0(benchmark_prefix, "methodId|mean")
  input_mean_check = checkboxInput(inputId = id, label = "Mean", value = TRUE)
  
  id = paste0(benchmark_prefix, "methodId|naive")
  input_naive_check = checkboxInput(inputId = id, label = "Naive", value = TRUE)
  
  id = paste0(benchmark_prefix, "methodId|snaive")
  input_snaive_check = checkboxInput(inputId = id, label = "Seasonal Naive", value = TRUE)
  
  id = paste0(benchmark_prefix, "methodId|drift")
  input_drift_check = checkboxInput(inputId = id, label = "Drift", value = TRUE)
  
  id = paste0(benchmark_prefix, "buttonPerformBenchmark")
  button_perform_benchmark = actionButton(inputId = id, label = "Perform Benchmark")
  
  id = paste0(benchmark_prefix, "selectErrorType")
  select_error_type = selectInput(id, "Error Type:", 
                                  c("Mean Square Error" = "mse",
                                    "Mean Absolute Error" = "mae"
                                  ), selected="mae"
  )
  
  row_1 = fluidRow(column(2, input_numeric_h), 
                   column(1, "Compare against : "),
                   column(1, input_mean_check), 
                   column(1, input_naive_check), 
                   column(1, input_snaive_check), 
                   column(1, input_drift_check),
                   column(3, select_error_type),
                   column(1, button_perform_benchmark)
  )
  
  # row_2 - graphs
  id = paste0(benchmark_prefix, "benchmarkPlot")
  graph_benchmark = plotOutput(id)
  row_2 = fluidRow(graph_benchmark)
  
  # row_3 - summary
  id = paste0(benchmark_prefix, "benchmarkTable")
  plot_benchmark = uiOutput(id)
  
  id = paste0(benchmark_prefix, "benchmarkError")
  text_benchmark_error = uiOutput(id)
  
  row_3 = fluidRow(column(6, plot_benchmark), 
                   column(6, text_benchmark_error)
  )
  
  tabPanel(title = "LRegression Benchmark", row_1, row_2, row_3)
  
}



## Server Functions
getBenchmarkResults=function(y_name="orders_rcvd", model_predictions, h=6, 
                             mean_model=TRUE, naive_model=TRUE, snaive_model=TRUE,
                             drift_model=TRUE, error_type="mae"){
  
  revenue_file = paste0(data_folder, "/", product_line, "/Revenue.csv")
  data = read.csv(revenue_file, header = TRUE, sep = ",")
  y = data[,y_name]
  
  t = ts(y, frequency=12, start=product_start_date, end=product_end_date)
  plot(t)
  
  last_ym = as.yearmon(paste0(product_end_date,collapse="-"))
  
  train_end_ym = format(last_ym - (h/12),"%Y-%m")
  train_end_ym = as.numeric(unlist(strsplit(train_end_ym,"-")))
  t_window = window(t, end=train_end_ym)
  
  valid_start_ym = format(last_ym - (h/12) + (1/12),"%Y-%m")
  valid_start_ym = as.numeric(unlist(strsplit(valid_start_ym,"-")))
  
  preview_start_ym = format(last_ym - ((2*h)/12),"%Y-%m")
  preview_start_ym = as.numeric(unlist(strsplit(preview_start_ym,"-")))
  preview_window = window(t, start=preview_start_ym)
  plot(preview_window)
  results = list(t=preview_window)
  
  train_indices = 1:(length(y)-h)
  y_actual = y[-train_indices]
  
  ## Average Method
  if(mean_model){
    predictions = meanf(t_window, h)$mean
    lines(predictions, col=2, lwd=2, lty=2)
    error = ifelse(error_type=="mae", 
                   mean(abs(predictions-y_actual)), mean((predictions-y_actual)^2))
    print(paste0("Prediction MAE for Average Method : ", error))
    
    results[['line_mean']] = predictions
    results[['error_mean']] = error
  }
  
  ## Naive Method
  if(naive_model){
    predictions = naive(t_window, h)$mean
    lines(predictions, col=3, lwd=2, lty=2)
    error = ifelse(error_type=="mae", 
                   mean(abs(predictions-y_actual)), mean((predictions-y_actual)^2))
    print(paste0("Prediction MAE for Naive Method : ", error))
    
    results[['line_naive']] = predictions
    results[['error_naive']] = error
  }
  
  ## Seasonal Naive Method
  if(snaive_model){
    predictions = snaive(t_window, h)$mean
    lines(predictions, col=4, lwd=2, lty=2)
    error = ifelse(error_type=="mae", 
                   mean(abs(predictions-y_actual)), mean((predictions-y_actual)^2))
    print(paste0("Prediction MAE for Seasonal Naive Method : ", error))
    
    results[['line_snaive']] = predictions
    results[['error_snaive']] = error
  }
  
  ## Drift Method
  if(drift_model){
    predictions = rwf(t_window, h, drift=TRUE)$mean
    lines(predictions, col=5, lwd=2, lty=2)
    error = ifelse(error_type=="mae", 
                   mean(abs(predictions-y_actual)), mean((predictions-y_actual)^2))
    print(paste0("Prediction MAE for Drift Method : ", error))
    
    results[['line_drift']] = predictions
    results[['error_drift']] = error
  }
  
  # Predictions by your model
  predictions = ts(model_predictions, frequency=12, 
                   start=valid_start_ym)
  lines(predictions, col=6, lwd=2, lty=2)
  error = ifelse(error_type=="mae", 
                 mean(abs(predictions-y_actual)), mean((predictions-y_actual)^2))
  print(paste0("Prediction MAE for ModelX : ", error))
  
  results[['line_modelX']] = predictions
  results[['error_modelX']] = error
  
  legend("topleft", lwd=2, lty=2, col=c(2,3,4,5,9),
         legend=c("Mean","Naive","Seasonal Naive","Drift","ModelX"))
  
  results
  
}


attachBenchmarkObservers = function(input, output, reactive_vars){
  
  benchmark_perform_benchmark = paste0(benchmark_prefix, "buttonPerformBenchmark")
  observeEvent(input[[benchmark_perform_benchmark]], {
    
    h = input[[paste0(benchmark_prefix, "h")]]
    mean_model = input[[paste0(benchmark_prefix,"methodId|mean")]]
    naive_model = input[[paste0(benchmark_prefix,"methodId|naive")]]
    snaive_model = input[[paste0(benchmark_prefix,"methodId|snaive")]]
    drift_model = input[[paste0(benchmark_prefix,"methodId|drift")]]
    error_type = input[[paste0(benchmark_prefix,"selectErrorType")]]
    
    fit_forecast = doRegression(input, reactive_vars[[MODEL_LINEAR_REGRESSION_VARS]], 
                                product_data_column, h=h)
    
    results = getBenchmarkResults(model_predictions=fit_forecast[['forecast']][,'fit'], 
                                  h=h, mean_model=mean_model, naive_model=naive_model,
                                  snaive_model=snaive_model, drift_model=drift_model,
                                  error_type=error_type)
    
    df_benchmark_fit[,BENCHMARK_LREGRESSION] <<- round(fit_forecast[['forecast']][,'fit'],2)
    
    ## Benchmark Forecast
    output_graph_benchmark = paste0(benchmark_prefix, "benchmarkPlot")
    output[[output_graph_benchmark]] = renderPlot({
      
      last_ym = as.yearmon(paste0(product_end_date,collapse="-"))
      valid_start_ym = format(last_ym - (h/12) + (1/12),"%Y-%m")
      valid_start_ym = as.numeric(unlist(strsplit(valid_start_ym,"-")))
      
      lwr = ts(fit_forecast[['forecast']][,'lwr'], frequency=12, 
               start=valid_start_ym)
      
      upr = ts(fit_forecast[['forecast']][,'upr'], frequency=12, 
               start=valid_start_ym)
      
      y_values = c(results[["t"]], lwr, upr)
      plot(results[["t"]], ylim=c(min(y_values), max(y_values)), ylab=product_data_column)
      
      models = vector('character')
      colors = c(2,3,4,5,9)
      index = 1
      for(result in names(results)){
        
        if(grepl("line_", result)){
          lines(results[[result]], col=colors[index], lwd=2, lty=2)
          
          model = unlist(strsplit(result,"_"))[2]
          models = c(models, model)
          
          index = index + 1
        }
        
      }
      lines(lwr, lwd=1, lty=3, col=45)
      lines(upr, lwd=1, lty=3, col=45)
      
      legend("topleft", lwd=2, lty=2, col=colors, legend=models)
      
    })
    
    ## Benchmark Forecast Table
    plot_benchmark = paste0(benchmark_prefix, "benchmarkTable")
    output[[plot_benchmark]] = renderUI({
      
      # Prediction Interval
      df = as.data.frame(fit_forecast[['forecast']])
      df = cbind(df, actual=tail(product_data,n=h))
      rows_values = sapply(1:nrow(df), function(i){
        row = df[i,]
        fit = round(row[['fit']], 2)
        actual = round(row[['actual']], 2)
        error = round(actual-fit, 2)
        lwr = round(row[['lwr']], 2)
        upr = round(row[['upr']], 2)
        interval = abs(upr-lwr)
        text = paste0("<tr><td>&nbsp;", i ,"&nbsp</td>",
                      "<td>&nbsp;", fit ,"&nbsp</td>",
                      "<td>&nbsp;", error ,"&nbsp</td>",
                      "<td>&nbsp;",lwr,"&nbsp;</td>",
                      "<td>&nbsp;",upr, "&nbsp;</td>",
                      "<td>&nbsp;", interval,"&nbsp;</td></tr>")
        text
      })
      rows_values = paste0(rows_values, collapse="")
      table_benchmark = paste0("<table><tr><th>#</th>",
                             "<th>Forecast</th><th>Error</th>",
                             "<th>Lower</th><th>Upper</th>",
                             "<th>Interval</th></tr>",
                             rows_values,"</table>")
      
      HTML(table_benchmark)
    })
    
    ## Benchmark Error
    plot_benchmark = paste0(benchmark_prefix, "benchmarkError")
    output[[plot_benchmark]] = renderUI({
      
      text_errors = vector('character')
      for(result in names(results)){
        
        if(grepl("error_", result)){
          
          model = unlist(strsplit(result,"_"))[2]
          model_summary = paste0(error_type, " for ", 
                                 model, " = ", results[[result]], "<br/>")
          text_errors = c(text_errors, model_summary)
        }
        
      }
      text_errors = paste0(text_errors, collapse="")
      HTML(text_errors)
    })
    
  })
}
