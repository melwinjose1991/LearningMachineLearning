print("Models :: TimeSeries :: Init")
library(forecast)
library(tseries)


## Globals
timeseries_prefix = paste0(models_prefix, "timeseries_")
preview_multiplier = 2



## UI Elements
getTimeSeriesUI = function(){
  
  # Row - 1 : Parameters
  id = paste0(timeseries_prefix, "h")
  input_numeric_h = numericInput(id, "In-Sample Forecast Period", 
                                 value=models_forecast_h, min=2, max=12, 
                                 step=1, width="60%")
  
  id = paste0(timeseries_prefix, "method")
  input_method = radioButtons(id, label="Method", selected="arima", inline=TRUE, 
                              choiceNames=c("Decompose-Naive", "Decompose-SNaive", 
                                            "Non-ARIMA","ARIMA"),
                              choiceValues=c("decompose-naive", "decompose-snaive",
                                             "non-arima","arima"))
  
  id = paste0(timeseries_prefix, "buildModel")
  input_forecast_button = actionButton(id, label="Build Model")
  
  row_1 = fluidRow(column(3, input_numeric_h),
                     column(7, input_method),
                     column(2, input_forecast_button))
  
  
  # Row-2 : Plot
  id = paste0(timeseries_prefix, "forecastPlot")
  output_forecast = plotOutput(id)
  row_2 = fluidRow(output_forecast)
  
  
  # Row-3 : Summary
  id = paste0(timeseries_prefix, "forecastTable")
  output_forecast_table = htmlOutput(id)
  
  id = paste0(timeseries_prefix, "forecastError")
  output_forecast_error = htmlOutput(id)
  
  row_3 = fluidRow(column(5, output_forecast_table),
                   column(4, output_forecast_error))
  
  tabPanel(title="Time Series", row_1, row_2, row_3)
}



## Server Function
tailForecast = function(pred, n=24){
  pred$x = tail(pred$x, n)
  pred$fitted = tail(pred$fitted, n)
  pred$residuals = tail(pred$residuals, n)
  pred
}

doAutoARIMA = function(train, h=12, in_sample_forecast=TRUE, actuals=NULL,
                       seasonal=TRUE, stepwise=TRUE, approximation=TRUE){
  print(paste0("Models :: Time-Series :: doAutoARIMA :: START"))
  
  arima_model = auto.arima(train, seasonal=seasonal, 
                           stepwise=stepwise, approximation=approximation)
  arima_model
  pred = forecast(arima_model, h=h)
  
  if(in_sample_forecast){
    train_mae = mean(abs(arima_model$residuals))
    valid_mae = mean(abs(actuals-pred$mean))
  }else{
    train_mae = mean(abs(arima_model$residuals))
    valid_mae = 0
  }
  print(paste0("train-mae:",train_mae," | valid-mae:",valid_mae))
  
  plot(arima_model$residuals)
  adf_test = adf.test(arima_model$residuals, alternative = "stationary")
  if(adf_test$p.value<=0.01){
    result_adf = "OK"
  }else{
    result_adf = "FAILED"
  }
  
  kpss_test = kpss.test(arima_model$residuals)
  if(kpss_test$p.value>0.01){
    result_kpss = "OK"
  }else{
    result_kpss = "FAILED"
  }

  print(paste0("Models :: Time-Series :: doAutoARIMA :: DONE"))  
  list(forecast_fit=pred, error = valid_mae,
       error_stationarity=c(result_adf, result_kpss))
}



attachTimeSeriesObservers = function(input, output){
  
  id = paste0(timeseries_prefix, "buildModel")
  observeEvent(input[[id]],{
    
    h = input[[paste0(timeseries_prefix, "h")]]
    id = paste0(timeseries_prefix, "method")
    
    data_ts = ts(product_data, frequency=12, 
                 start=product_start_date, end=product_end_date)
    
    last_ym = as.yearmon(paste0(product_end_date,collapse="-"))
    train_end_ym = format(last_ym - (h/12),"%Y-%m")
    train_end_ym = as.numeric(unlist(strsplit(train_end_ym,"-")))
    train = window(data_ts, end=train_end_ym)  
    
    if(input[[id]]=="arima"){
      result = doAutoARIMA(train, h, actuals=tail(data_ts, n=h))
    }
    
    ## Forecast Plot
    id = paste0(timeseries_prefix, "forecastPlot")
    output[[id]] = renderPlot({
      last_ym = as.yearmon(paste0(product_end_date,collapse="-"))
      
      preview_start_ym = format(last_ym - ((preview_multiplier*h)/12),"%Y-%m")
      preview_start_ym = as.numeric(unlist(strsplit(preview_start_ym,"-")))
      preview_window = window(data_ts, start=preview_start_ym)
      
      tailed_results = tailForecast(result[['forecast_fit']], h*2)
      
      all_points = c(tailed_results$mean, preview_window,
                     tailed_results$upper[,2], tailed_results$lower[,2])
      y_range = c(min(all_points), max(all_points))
      
      plot(preview_window, ylim=y_range, ylab=product_data_column)
      lines(tailed_results$mean, col=50, lwd=3, lty=2)
      lines(tailed_results$lower[,2], col=45, lwd=3, lty=3)
      lines(tailed_results$upper[,2], col=45, lwd=3, lty=3)
    })
    
    ## Forecast Table
    df_benchmark_fit[,BENCHMARK_TIMESERIES] <<- round(result[["forecast_fit"]]$mean,2)
    id = paste0(timeseries_prefix, "forecastTable")
    output[[id]] = renderUI({
      
      df = data.frame(fit=result[["forecast_fit"]]$mean,
                      lwr=result[["forecast_fit"]]$lower[,2],
                      upr=result[["forecast_fit"]]$upper[,2],
                      actual = tail(product_data, n=h))
      
      text_forecast = sapply(1:nrow(df), function(i){
        row = df[i,]
        fit = round(row[['fit']], 2)
        actual = round(row[['actual']], 2)
        error = round(fit-actual, 2)
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
      
      text_forecast = paste0(text_forecast, collapse="")
      text_forecast = paste0("<table><tr><th>#</th>",
                             "<th>Forecast</th><th>Error</th>",
                             "<th>Lower</th><th>Upper</th>",
                             "<th>Interval</th></tr>",
                             text_forecast,"</table>")
      
      HTML(text_forecast)
    })
    
    ## Forecast Error
    text_error = paste0("MAE : ", round(result[['error']],2) )
    id = paste0(timeseries_prefix, "forecastError")
    output[[id]] = renderUI({
      HTML(text_error)
    })
    
  })
  
}