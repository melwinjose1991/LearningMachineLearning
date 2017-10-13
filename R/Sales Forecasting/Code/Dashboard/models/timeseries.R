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
                                 value=12, min=2, max=12, 
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
buildSTLModel = function(train, h=6, forecast_method="naive"){
  
  t.windows = 1:15
  s.windows = c(7,9,11,13)
  maes = vector('numeric')
  configs_t = vector('numeric')
  configs_s = vector('numeric')
  window_ts = train
  
  for(s.window in s.windows){
    for(t.window in t.windows){
      
      # plot(window_ts)
      fit <- stl(window_ts, t.window=t.window, s.window=s.window, robust=TRUE)
      
      if(forecast_method=="naive"){
        sa_adj <- seasadj(fit)
        forecast = naive(sa_adj, h=h)  
      }else if(forecast_method=="snaive"){
        forecast = forecast(fit, method="naive", h=h)
      }
      
      # plot(forecast)
      # print(forecast$mean)
      
      mae = mean(abs(c(tail(train,n=h))-forecast$mean))
      maes = c(maes, mae)
      
      configs_t = c(configs_t, t.window)
      configs_s = c(configs_s, s.window)
      
    }
  }
  
  maes[which.min(maes)]
  
  t.window = configs_t[which.min(maes)]
  s.window = configs_s[which.min(maes)]
  fit = stl(window_ts, t.window=t.window, s.window=s.window, robust=TRUE)
  if(forecast_method=="naive"){
    sa_adj = seasadj(fit)
    forecast = naive(sa_adj, h=h)  
  }else if(forecast_method=="snaive"){
    forecast = forecast(fit, method="naive", h=h)
  }
  plot(forecast)
  mae = mean(abs(c(tail(train,n=h))-forecast$mean))
  
  list(forecast_fit=forecast, error=mae)
}

doSES = function(train, h=6){
  print(paste0("Models :: Time-Series :: doSES() :: START"))
  
  # data_ts = ts(product_data, frequency=12)
  # window_ts = window(data_ts, end=product_last_year_index+((12-(h+1))/12))  
  window_ts = train
  fit = ses(window_ts, initial="simple", h=h)
  mae = mean(abs(c(tail(train,n=h))-c(fit$mean)))
  
  print(paste0("Models :: Time-Series :: doSES() :: DONE"))
  list(forecast_fit=fit, error=mae)
}

doHolts = function(train, h=6, damped=FALSE, t_multiplicative=FALSE){
  print(paste0("Models :: Time-Series :: doHolts() :: START"))
  
  # data_ts = ts(product_data, frequency=12)
  # window_ts = window(data_ts, end=product_last_year_index+((12-(h+1))/12))
  window_ts = train
  fit = holt(window_ts, h=h, damped=damped, 
             exponential=t_multiplicative)
  #plot(data_ts)
  #lines(fit$fitted, col="red", lwd=2, lty=2)
  #lines(fit$mean, col="green", lwd=2, lty=2)
  mae = mean(abs(c(tail(train,n=h))-c(fit$mean)))

  print(paste0("Models :: Time-Series :: doHolts() :: DONE"))
  list(forecast_fit=fit, error=mae)
}

doHW = function(train, h=6, seasonal="additive", damped=FALSE, t_multiplicative=FALSE){
  print(paste0("Models :: Time-Series :: doHW() :: START"))
  
  #data_ts = ts(product_data, frequency=12)
  #window_ts = window(data_ts, end=product_last_year_index+((12-(h+1))/12))
  window_ts = train
  fit = hw(window_ts, h=h, seasonal=seasonal,
                        damped=damped, 
                        exponential=t_multiplicative)
  #plot(data_ts, lwd=2)
  #lines(fit_multi_damped$fitted, col="red", lwd=2, lty=2)
  #lines(fit_multi_damped$mean, col="green", lwd=2, lty=2)
  #train_mae = mean(abs(fit_multi_damped$residuals))
  mae = mean(abs(c(tail(train,n=h)-c(fit$mean))))
  
  print(paste0("Models :: Time-Series :: doHW() :: DONE"))
  list(forecast_fit=fit, error=mae)
}

getBestNonArimaModelError = function(train, h=6){
  print(paste0("Models :: Time-Series :: getBestNonArimaModelError() :: START"))
  
  best_error = vector('numeric')
  
  result_ses = doSES(train, h)
  best_error = c(best_error, result_ses[['error']])
  
  ## Holts  
  result_holts = doHolts(train, h, damped=FALSE, t_multiplicative=FALSE)
  best_error = c(best_error, result_holts[['error']])
  
  result_holts_m = doHolts(train, h, damped=FALSE, t_multiplicative=TRUE)
  best_error = c(best_error, result_holts_m[['error']])
  
  result_holts_d = doHolts(train, h, damped=TRUE, t_multiplicative=FALSE)
  best_error = c(best_error, result_holts_d[['error']])
  
  result_holts_dm = doHolts(train, h, damped=TRUE, t_multiplicative=TRUE)
  best_error = c(best_error, result_holts_dm[['error']])
  
  ## Holt's Winters
  result_hw_a = doHW(train, h, seasonal="additive", damped=FALSE, t_multiplicative=FALSE)
  best_error = c(best_error, result_hw_a[['error']])
  
  #result_hw_am = doHW(train, h, seasonal="additive", damped=FALSE, t_multiplicative=TRUE)
  
  result_hw_ad = doHW(train, h, seasonal="additive", damped=TRUE, t_multiplicative=FALSE)
  best_error = c(best_error, result_hw_ad[['error']])
  
  #result_hw_adm = doHW(train, h, seasonal="additive", damped=TRUE, t_multiplicative=TRUE)
  
  result_hw_x = doHW(train, h, seasonal="multiplicative", damped=FALSE, t_multiplicative=FALSE)
  best_error = c(best_error, result_hw_x[['error']])
  
  result_hw_xm = doHW(train, h, seasonal="multiplicative", damped=FALSE, t_multiplicative=TRUE)
  best_error = c(best_error, result_hw_xm[['error']])
  
  result_hw_xd = doHW(train, h, seasonal="multiplicative", damped=TRUE, t_multiplicative=FALSE)
  best_error = c(best_error, result_hw_xd[['error']])
  
  result_hw_xdm = doHW(train, h, seasonal="multiplicative", damped=TRUE, t_multiplicative=TRUE)
  best_error = c(best_error, result_hw_xdm[['error']])

  print(paste0("Models :: Time-Series :: getBestNonArimaModelError() :: END"))
  switch( which.min(best_error),
         "1" = result_ses,
         
         "2" = result_holts,
         "3" = result_holts_d,
         "4" = result_holts_m,
         "5" = result_holts_dm,
         
         "6" = result_hw_a,
         "7" = result_hw_ad,
         "8" = result_hw_x,
         "9" = result_hw_xm,
         "10" = result_hw_xd,
         "11" = result_hw_xdm
  )
  
}

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
    
    if(input[[id]]=="decompose-naive"){
      result = buildSTLModel(train, h=h, forecast_method="naive")
      
    }else if(input[[id]]=="decompose-snaive"){  
      result = buildSTLModel(train, h=h, forecast_method="snaive")
      
    }else if(input[[id]]=="non-arima"){
      result = getBestNonArimaModelError(train, h=h)
      
    }else if(input[[id]]=="arima"){
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