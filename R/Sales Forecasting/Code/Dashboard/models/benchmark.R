print("Models :: Benchmark :: Init")

library(forecast)



## Globals
benchmark_prefix = paste0(models_prefix, "benchmark_")



## UI Elements
getBenchmarkUI = function(){
  
  # row_1 - options
  id = paste0(benchmark_prefix, "h")
  input_numeric_h = numericInput(id, "Forecast Period", value=2, min = 2, max = 6, step = 1, width = "50%")
  
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
                                  )
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
  id = paste0(benchmark_prefix, "graphBenchmark")
  output_graph_benchmark = plotOutput(id)
  row_2 = fluidRow(output_graph_benchmark)
  
  # row_3 - summary
  id = paste0(benchmark_prefix, "summaryBenchmark")
  output_summary_benchmark = uiOutput(id)
  row_3 = fluidRow(output_summary_benchmark)
  
  tabPanel(title = "Benchmark", row_1, row_2, row_3)
  
}



## Server Functions
getBenchmarkResults=function(y_name="orders_rcvd", model_predictions, h=6, 
                             mean_model=TRUE, naive_model=TRUE, snaive_model=TRUE,
                             drift_model=TRUE, error_type="mae"){
  
  revenue_file = paste0(data_folder, "/", product_line, "/Revenue.csv")
  data = read.csv(revenue_file, header = TRUE, sep = ",")
  y = data[,y_name]
  
  t = ts(y, frequency=12)
  plot(t)
  results = list(t=t)
  t_window = window(t, end=4+((12-(h+1))/12))
  
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
  predictions = ts(model_predictions, frequency = 12, start=4+((12-h)/12))
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
