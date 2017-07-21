library(shiny)

product = "2401"

server = function(input, output, session) {
  
  reactive_vars = reactiveValues()
  
  ### Products
  attachProductsObservers(input, output, session, reactive_vars)
  
  ### FRED
  attachFREDObservers(input)
  
  ### Feature Selection > Features
  populateFeatures(input, output, session)

  ### Feature Selection > Feature Selection
  attachObservers(input, output, session, reactive_vars)
  
  ### Models > Regression
  regression_build_regression = paste0(regression_prefix, "buttonBuildRegression")
  observeEvent(input[[regression_build_regression]], {
    fit = doRegression(input, reactive_vars[['selected_vars']], product_data_column)
    reactive_vars[['forecast_model']] = fit[['regression']]
    
    text_tests_id = paste0(regression_prefix, "testResults")
    output[[text_tests_id]] = renderUI({
      list_failed_tests = fit[['failed_tests']]
      no_of_failed_tests = sum(lengths(list_failed_tests))
      text_failed_tests_count = tags$h4(paste0("#Test Failed : ", no_of_failed_tests), 
                                  style=paste0("color:", test_color_code[no_of_failed_tests+1],"; font-weight: bold;") )
      
      text_failed_tests = vector('character')
      for(index in names(list_failed_tests)){
        text_failed_test = list_failed_tests[[index]]
        text_failed_tests = c(text_failed_tests, text_failed_test)
      }
      text_failed_tests = paste0(text_failed_tests, collapse="<br/>")
      
      text_reg_summary_F = paste0("<br/><br/>F-statistic : ", summary(fit[['regression']])$fstatistic[1], "<br/>")
      text_reg_summary_R = paste0("R2 : ", summary(fit[['regression']])$r.squared, "<br/>")
      text_reg_summary = paste0(text_reg_summary_F, text_reg_summary_R)
      
      HTML(paste0(text_failed_tests_count, text_failed_tests, text_reg_summary))
    })
    
    output_regression_graph_1 = paste0(regression_prefix, "graphResidualVsFitted")
    output[[output_regression_graph_1]] = renderPlot({
      plot(fit[["regression"]], which=1)
    })
    
    output_regression_graph_2 = paste0(regression_prefix, "graphQQ")
    output[[output_regression_graph_2]] = renderPlot({
      plot(fit[["regression"]], which=2)
    })
    
    output_regression_graph_3 = paste0(regression_prefix, "graphStdResVsFitted")
    output[[output_regression_graph_3]] = renderPlot({
      plot(fit[["regression"]], which=3)
    })
    
    output_regression_graph_4 = paste0(regression_prefix, "graphStdResVsLeverage")
    output[[output_regression_graph_4]] = renderPlot({
      plot(fit[["regression"]], which=4)
    })

    output_regression_graph_5 = paste0(regression_prefix, "graphObservedVsFitted")
    output[[output_regression_graph_5]] = renderPlot({
      
      obs_to_exclude = input[[paste0(products_prefix, product_line, "|avoid")]]
      obs_to_exclude = unlist(strsplit(obs_to_exclude,","))
      obs_to_exclude = as.numeric(obs_to_exclude)
      use_rows = setdiff(1:length(product_data), obs_to_exclude)
      
      plot(product_data[use_rows], fit[["regression"]]$fitted.values,
           xlab="Actuals", ylab="Fitted" )
      lines(product_data[use_rows], product_data[use_rows])
    })
    
    fillVariableTable(session, fit[['regression']])
    
  })
  
  ### Models > TimeSeries
  attachTimeSeriesObservers(input, output)
  
  ### Models > Benchmark
  attachBenchmarkObservers(input, output, reactive_vars)
  
  
  
  ### Forecast > forecast
  forecast_do_forecast = paste0(forecast_prefix, "buttonForecast")
  observeEvent(input[[forecast_do_forecast]], {
    print(">>> Forecasting <<<")
    
    h = no_of_forecast # Has to be user entered
    mean_model = FALSE #input[[paste0(forecast_prefix,"methodId|mean")]]
    naive_model = FALSE #input[[paste0(forecast_prefix,"methodId|naive")]]
    snaive_model = FALSE #input[[paste0(forecast_prefix,"methodId|snaive")]]
    drift_model = FALSE #input[[paste0(forecast_prefix,"methodId|drift")]]
    
    forecast_model = reactive_vars[['forecast_model']]
    for(var in reactive_vars[['selected_vars']]){
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
      
      f_interval = abs(results[['line_modelX_upr']] - results[['line_modelX_lwr']])
      df = data.frame(forecast = results[['line_modelX']],
                       lwr = results[['line_modelX_lwr']],
                       upr = results[['line_modelX_upr']],
                       interval = round(f_interval,2))
      
      text_forecast = sapply(1:nrow(df), function(i){
        row = df[i,]
        fit = round(row[['forecast']], 2)
        lwr = round(row[['lwr']], 2)
        upr = round(row[['upr']], 2)
        interval = round(row[['interval']], 2)
        
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

