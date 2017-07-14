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
      
      obs_to_exclude = input[[paste0(products_prefix, product, "|avoid")]]
      obs_to_exclude = as.numeric(unlist(strsplit(obs_to_exclude,",")))
      use_rows = setdiff(1:nrow(data), obs_to_exclude)
      
      plot(product_data[use_rows], fit[["regression"]]$fitted.values,
           xlab="Actuals", ylab="Fitted" )
    })
    
    fillVariableTable(session, fit[['regression']])
    
  })
  
  ### Models > TimeSeries
  attachTimeSeriesObservers(input, output)
  
  ### Models > Benchmark
  benchmark_perform_benchmark = paste0(benchmark_prefix, "buttonPerformBenchmark")
  observeEvent(input[[benchmark_perform_benchmark]], {
    
    h = input[[paste0(benchmark_prefix, "h")]]
    mean_model = input[[paste0(benchmark_prefix,"methodId|mean")]]
    naive_model = input[[paste0(benchmark_prefix,"methodId|naive")]]
    snaive_model = input[[paste0(benchmark_prefix,"methodId|snaive")]]
    drift_model = input[[paste0(benchmark_prefix,"methodId|drift")]]
    error_type = input[[paste0(benchmark_prefix,"selectErrorType")]]
    
    fit_forecast = doRegression(input, reactive_vars[['selected_vars']], 
                                product_data_column, h=h)
  
    
    results = getBenchmarkResults(model_predictions=fit_forecast[['forecast']], h = h,
                                  mean_model=mean_model, naive_model=naive_model,
                                  snaive_model=snaive_model, drift_model=drift_model,
                                  error_type=error_type)
    
    
    ## Plotting graphs
    output_graph_benchmark = paste0(benchmark_prefix, "graphBenchmark")
    output[[output_graph_benchmark]] = renderPlot({
      
      plot(results[["t"]])
      
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
      
      legend("topleft", lwd=2, lty=2, col=colors, legend=models)
      
    })
    
    ## Reporting summary
    output_summary_benchmark = paste0(benchmark_prefix, "summaryBenchmark")
    output[[output_summary_benchmark]] = renderUI({
      
      summary_text = list()
      for(result in names(results)){
        
        if(grepl("error_", result)){
          
          model = unlist(strsplit(result,"_"))[2]
          
          model_summary = paste0(error_type, " for ", model, " = ", results[[result]])
          html_element = tags$p(model_summary)
          summary_text = list(summary_text, list(html_element))
        }
        
      }
      summary_text
      
    })
    
  })
  
  
  
  ### Forecast > forecast
  forecast_do_forecast = paste0(forecast_prefix, "buttonForecast")
  observeEvent(input[[forecast_do_forecast]], {
    print(">>> Forecasting <<<")
    
    h = 1 # Has to be user entered
    mean_model = FALSE #input[[paste0(forecast_prefix,"methodId|mean")]]
    naive_model = FALSE #input[[paste0(forecast_prefix,"methodId|naive")]]
    snaive_model = FALSE #input[[paste0(forecast_prefix,"methodId|snaive")]]
    drift_model = FALSE #input[[paste0(forecast_prefix,"methodId|drift")]]
    
    forecast_model = reactive_vars[['forecast_model']]
    for(var in reactive_vars[['selected_vars']]){
      if(!exists("variable_values")){
        variable_values = data.frame(row=1:h)
      }
      var_id = paste0(forecast_prefix, "varId|", var, "|value")
      if(is.logical(input[[var_id]])){
        variable_values[,var] = ifelse(input[[var_id]],1,0)
      }else{
        variable_values[,var] = as.numeric(input[[var_id]])
      }
    }
    
    results = getForecastResults(forecast_model=forecast_model, 
                                 input_variables=variable_values, 
                                 h=h, mean_model=mean_model, naive_model=naive_model,
                                 snaive_model=snaive_model, drift_model=drift_model)
    
    
    ## Plotting graphs
    output_graph_forecast = paste0(forecast_prefix, "forecastPlot")
    output[[output_graph_forecast]] = renderPlot({
      
      plot(results[["t"]])
      
      models = vector('character')
      colors = c(2,3,4,5,9)
      index = 1
      for(result in names(results)){
        
        if(grepl("line_", result)){
          if( grepl("upr",result) | grepl("lwr",result) ){
            lines(results[[result]], col="blue", lty=2)
          }else{
            
            lines(results[[result]], col=colors[index], lwd=2, lty=2)
            
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
      res_forecast = paste0("Forecast : ", results[['line_modelX']][2])
      res_lower    = paste0("Lower : ", results[['line_modelX_lwr']][1])
      res_upper    = paste0("Upper : ", results[['line_modelX_upr']][1])
      res_interval = paste0("Interval : ",
                            (results[['line_modelX_upr']][1] - results[['line_modelX_lwr']][1]))
      HTML(paste(res_forecast, res_upper, res_lower, res_interval, sep="<br/>"))
    })
    
  })
  
}

