print("Forecast :: forecast_timeseries.R :: Init")



## Globals
forecast_timeseries_prefix = paste0(forecast_tab_prefix, "forecastTimeSeries_")



## UI Elements
getForecastTimeSeriesUI = function(){
  
  # row 1 - Buttons
  button_forecast_id = paste0(forecast_timeseries_prefix, "buttonForecast")
  button_forecast = actionButton(inputId = button_forecast_id,
                                 label = "Forecast")
  row_1 = fluidRow(button_forecast)
  
  
  # row 2 - plots
  id = paste0(forecast_timeseries_prefix, "forecastPlot")
  column_graph_forecast = column(width=8,plotOutput(id))
  
  id = paste0(forecast_timeseries_prefix, "forecastValue")
  column_value_forecast = column(width=4, htmlOutput(id))
  
  row_2 = fluidRow(column_graph_forecast, column_value_forecast)
  
  
  tabPanel(title = "Time-Series", row_1, row_2)
  
}



## Server Functions
attachForecastTimeSeriesObservers = function(input, output, session){
  
  id_1 = paste0(forecast_timeseries_prefix, "buttonForecast")
  observeEvent(input[[id_1]], {
    
    data_ts = ts(product_data, frequency=12, 
                 start=product_start_date, end=product_end_date)
    
    train = window(data_ts, end=product_end_date)
    
    result = doAutoARIMA(train, no_of_forecast, in_sample_forecast=FALSE)
    
    # Converting results to data.frame for iterability and ensembling
    h = no_of_forecast
    f_interval = abs(result[["forecast_fit"]]$upper[,2] - result[["forecast_fit"]]$lower[,2])
    df = data.frame(fit=result[["forecast_fit"]]$mean,
                    lwr=result[["forecast_fit"]]$lower[,2],
                    upr=result[["forecast_fit"]]$upper[,2],
                    interval=f_interval)
    
    reactive_vars[[FORECAST_TIMESERIES]] = df
    
    
    # Plotting forecast
    id_2 = paste0(forecast_timeseries_prefix, "forecastPlot")
    output[[id_2]] = renderPlot({
      
      last_ym = as.yearmon(paste0(product_end_date,collapse="-"))
      forecast_end_ym = format(last_ym +(no_of_forecast/12), "%Y-%m")
      forecast_end_ym = as.numeric(unlist(strsplit(forecast_end_ym, "-")))
      plot_window = ts(c(product_data, rep(NA, no_of_forecast)), frequency=12, 
                                 start=product_start_date, end=forecast_end_ym)
      
      all_points = c(result$mean, product_data, result$upper[,2], result$lower[,2])
      y_range = c(min(all_points), max(all_points))
      
      plot(plot_window, ylim=y_range)
      lines(result$forecast_fit$mean, col=50, lwd=3, lty=2)
      lines(result$forecast_fit$lower[,2], col=45, lwd=3, lty=3)
      lines(result$forecast_fit$upper[,2], col=45, lwd=3, lty=3)
    })
    
    
    # Forecast values
    id_3 = paste0(forecast_timeseries_prefix, "forecastValue")
    output[[id_3]] = renderUI({
      
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
      
      HTML(text_forecast)
      
    })
    
    
  })
  
}