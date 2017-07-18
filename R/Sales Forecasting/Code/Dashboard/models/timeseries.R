print("Models :: TimeSeries :: Init")
library(forecast)



## Globals
timeseries_prefix = paste0(models_prefix, "timeseries_")



## UI Elements
getTimeSeriesUI = function(){
  
  # Row - 1 : Parameters
  id = paste0(timeseries_prefix, "h")
  input_numeric_h = numericInput(id, "In-Sample Forecast Period", 
                                           value=12, min=2, max=12, step=1, width="60%")
  
  id = paste0(timeseries_prefix, "method")
  input_method = radioButtons(id, label="Method", selected="decompose-snaive", inline=TRUE, 
                              choiceNames=c("Decompose-Naive", "Decompose-SNaive", 
                                            "Non-ARIMA","ARIMA"),
                              choiceValues=c("decompose-naive", "decompose-snaive",
                                             "non-arima","arima"))
  
  row_1_1 = fluidRow(column(4, input_numeric_h),
                     column(8, input_method))
    
  
  id = paste0(timeseries_prefix, "hasTrend")
  input_trend = checkboxInput(id, "Trend ?", value=TRUE)
  id = paste0(timeseries_prefix, "trendWindow")
  input_numeric_trend = numericInput(id, "Trend Window", 
                                 value=3, min=0, max=12, step=1, width="50%")
  
  
  id = paste0(timeseries_prefix, "hasSeasonality")
  input_season = checkboxInput(id, "Seasonality ?", value=TRUE)
  id = paste0(timeseries_prefix, "seasonalityWindow")
  input_numeric_season = numericInput(id, "Seasonal Window", 
                                     value=3, min=0, max=12, step=1, width="50%")
  
  row_1_2 = fluidRow(column(2, input_trend),
                     column(4, input_numeric_trend),
                     column(2, input_season),
                     column(4, input_numeric_season))
  
  id = paste0(timeseries_prefix, "buildModel")
  input_forecast_button = actionButton(id, label="Build Model")
  
  row_1 = fluidRow(row_1_1, input_forecast_button)
  
  # Row-2 : Plot
  id = paste0(timeseries_prefix, "forecastPlot")
  output_forecast = plotOutput(id)
  row_2 = fluidRow(output_forecast)
  
  # Row-3 : Summary
  id = paste0(timeseries_prefix, "forecastSummary")
  output_summary = htmlOutput(id)
  row_3 = fluidRow(output_summary)
  
  tabPanel(title="Time Series", row_1, row_2, row_3)
}


## Server Function
buildSTLModel = function(h=6, forecast_method="naive"){
  
  t.windows = 1:15
  s.windows = c(7,9,11,13)
  maes = vector('numeric')
  configs_t = vector('numeric')
  configs_s = vector('numeric')
  data_ts = ts(product_data, frequency=12)
  
  window_ts = window(data_ts, end=product_last_year_index+((12-(h+1))/12))  
  
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
      
      mae = mean(abs(tail(data_ts,n=h)-forecast$mean))
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
  mae = mean(abs(tail(data_ts,n=h)-forecast$mean))
  
  list(forecast_fit=forecast, error=mae)
}

doSES = function(h=6){
  print(paste0("Models :: Time-Series :: doSES() :: START"))
  
  data_ts = ts(product_data, frequency=12)
  window_ts = window(data_ts, end=product_last_year_index+((12-(h+1))/12))  
  fit = ses(window_ts, initial="simple", h=h)
  mae = mean(abs(tail(data_ts,n=h)-fit$mean))
  
  print(paste0("Models :: Time-Series :: doSES() :: DONE"))
  list(forecast_fit=fit, error=mae)
}

doHolts = function(h=6, damped=FALSE, t_multiplicative=FALSE){
  print(paste0("Models :: Time-Series :: doHolts() :: START"))
  
  data_ts = ts(product_data, frequency=12)
  window_ts = window(data_ts, end=product_last_year_index+((12-(h+1))/12))
  fit = holt(window_ts, h=h, damped=damped, 
             exponential=t_multiplicative)
  #plot(data_ts)
  #lines(fit$fitted, col="red", lwd=2, lty=2)
  #lines(fit$mean, col="green", lwd=2, lty=2)
  mae = mean(abs(tail(data_ts,n=h)-fit$mean))

  print(paste0("Models :: Time-Series :: doHolts() :: DONE"))
  list(forecast_fit=fit, error=mae)
}

doHW = function(h=6, seasonal="additive", damped=FALSE, t_multiplicative=FALSE){
  print(paste0("Models :: Time-Series :: doHW() :: START"))
  
  data_ts = ts(product_data, frequency=12)
  window_ts = window(data_ts, end=product_last_year_index+((12-(h+1))/12))
  fit = hw(window_ts, h=h, seasonal=seasonal,
                        damped=damped, 
                        exponential=t_multiplicative)
  #plot(data_ts, lwd=2)
  #lines(fit_multi_damped$fitted, col="red", lwd=2, lty=2)
  #lines(fit_multi_damped$mean, col="green", lwd=2, lty=2)
  #train_mae = mean(abs(fit_multi_damped$residuals))
  mae = mean(abs(tail(data_ts,n=h)-fit$mean))
  
  print(paste0("Models :: Time-Series :: doHW() :: DONE"))
  list(forecast_fit=fit, error=mae)
}

getBestNonArimaModelError = function(h=6){
  print(paste0("Models :: Time-Series :: getBestNonArimaModelError() :: START"))
  
  best_error = vector('numeric')
  
  result_ses = doSES(h)
  best_error = c(best_error, result_ses[['error']])
  
  ## Holts  
  result_holts = doHolts(h, damped=FALSE, t_multiplicative=FALSE)
  best_error = c(best_error, result_holts[['error']])
  
  result_holts_m = doHolts(h, damped=FALSE, t_multiplicative=TRUE)
  best_error = c(best_error, result_holts_m[['error']])
  
  result_holts_d = doHolts(h, damped=TRUE, t_multiplicative=FALSE)
  best_error = c(best_error, result_holts_d[['error']])
  
  result_holts_dm = doHolts(h, damped=TRUE, t_multiplicative=TRUE)
  best_error = c(best_error, result_holts_dm[['error']])
  
  ## Holt's Winters
  result_hw_a = doHW(h, seasonal="additive", damped=FALSE, t_multiplicative=FALSE)
  best_error = c(best_error, result_hw_a[['error']])
  
  #result_hw_am = doHW(h, seasonal="additive", damped=FALSE, t_multiplicative=TRUE)
  
  result_hw_ad = doHW(h, seasonal="additive", damped=TRUE, t_multiplicative=FALSE)
  best_error = c(best_error, result_hw_ad[['error']])
  
  #result_hw_adm = doHW(h, seasonal="additive", damped=TRUE, t_multiplicative=TRUE)
  
  result_hw_x = doHW(h, seasonal="multiplicative", damped=FALSE, t_multiplicative=FALSE)
  best_error = c(best_error, result_hw_x[['error']])
  
  result_hw_xm = doHW(h, seasonal="multiplicative", damped=FALSE, t_multiplicative=TRUE)
  best_error = c(best_error, result_hw_xm[['error']])
  
  result_hw_xd = doHW(h, seasonal="multiplicative", damped=TRUE, t_multiplicative=FALSE)
  best_error = c(best_error, result_hw_xd[['error']])
  
  result_hw_xdm = doHW(h, seasonal="multiplicative", damped=TRUE, t_multiplicative=TRUE)
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

doAutoARIMA = function(h=12, use_box_cox=FALSE, seasonal=TRUE){
  print(paste0("Models :: Time-Series :: doAutoARIMA :: START"))
  #use_box_cox = FALSE
  #seasonal = TRUE
  
  data_ts = ts(product_data, frequency=12)
  
  if(use_box_cox==TRUE){
    lambda = BoxCox.lambda(data_ts)
    data_ts_star = BoxCox(data_ts, lambda)
    print(lambda)
  }else{
    data_ts_star = data_ts
  }
  #plot(data_ts_star)
  
  train_window = window(data_ts_star, end=13+((12-(h+1))/12))  
  arima_model = auto.arima(train_window,seasonal=seasonal, 
                           stepwise=FALSE, approximation=FALSE)
  arima_model
  pred = forecast(arima_model, h=h)
  #plot(pred)
  
  if(use_box_cox==TRUE){
    train_mae = mean(abs(lambda^arima_model$fitted - data_ts[1:(length(data_ts)-h)]))
    valid_mae = mean(abs(tail(data_ts,n=h)-lambda^pred$mean))
  }else{
    train_mae = mean(abs(arima_model$residuals))
    valid_mae = mean(abs(tail(data_ts,n=h)-pred$mean))
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
    
    if(input[[id]]=="decompose-naive"){
      result = buildSTLModel(h=h, forecast_method="naive")
      
    }else if(input[[id]]=="decompose-snaive"){  
      result = buildSTLModel(h=h, forecast_method="snaive")
      
    }else if(input[[id]]=="non-arima"){
      result = getBestNonArimaModelError(h=h)
      
    }else if(input[[id]]=="arima"){
      result = doAutoARIMA(h)
    }
    
    id = paste0(timeseries_prefix, "forecastPlot")
    output[[id]] = renderPlot({
      plot(result[['forecast_fit']])
    })
    
    id = paste0(timeseries_prefix, "forecastSummary")
    output[[id]] = renderUI({
      
      text_error = paste0("MAE : ", result[['error']])
      
      df = data.frame(fit=result[["forecast_fit"]]$mean,
                      lwr=result[["forecast_fit"]]$lower[,2],
                      upr=result[["forecast_fit"]]$upper[,2])
      
      text_forecast = sapply(1:nrow(df), function(i){
        row = df[i,]
        fit = round(row[['fit']], 2)
        lwr = round(row[['lwr']], 2)
        upr = round(row[['upr']], 2)
        interval = abs(upr-lwr)
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
      
      text_summary = paste0(text_error, "<br/>", text_forecast)
      HTML(text_summary)
    })
    
  })
  
}