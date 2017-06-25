library(shiny)

product = "2401"

server = function(input, output, session) {
  
  reactive_vars = reactiveValues()
  
  ### Feature Selection > Features
  output$all_features_box = renderUI({
    lapply(
      unique(meta_data$sub_category_name),
      FUN = function(x)
        fluidRow(getGroupCheckBoxInput(x))
    )
  })
  
  groups = unique(meta_data$sub_category_name)
  lapply(groups, function(group) {
    select_all_id = paste0(all_features_prefix, "selectAllId|", group)
    show_button_id = paste0(all_features_prefix, "showButtonId|", group)
    
    observeEvent(input[[select_all_id]], {
      all_features = vector('character')
      for (key in names(input)) {
        if (grepl(paste0("_fid|", group), key)) {
          all_features = c(all_features, key)
        }
      }
      
      if (input[[select_all_id]]) {
        lapply(all_features, function(fid) {
          updateCheckboxInput(session, fid, value = TRUE)
        })
      } else{
        lapply(all_features, function(fid) {
          updateCheckboxInput(session, fid, value = FALSE)
        })
      }
      
    })
    
    observeEvent(input[[show_button_id]], {
      print(paste0("button : ", input[[show_button_id]]))
      if (input[[show_button_id]] %% 2 == 0) {
        showElement(paste0(all_features_prefix, "divId_", group))
        updateActionButton(session, show_button_id, label = "Hide")
      } else{
        hideElement(paste0(all_features_prefix, "divId_", group))
        updateActionButton(session, show_button_id, label = "Show")
      }
    })
    
  })
  
  
  
  ### Feature Selection > Feature Selection
  featureSelection_LASSO = paste0(feature_selection_prefix, "buttonLASSO")
  observeEvent(input[[featureSelection_LASSO]], {
    fit_and_coefs = readData(reactiveValuesToList(input))
    
    output_LASSOgraph = paste0(feature_selection_prefix, "outputLASSOGraph")
    output[[output_LASSOgraph]] = renderPlot({
      plot(fit_and_coefs[["fit"]])
    })
    
    output_LASSOcoefs = paste0(feature_selection_prefix, "outputLASSOCoefs")
    output[[output_LASSOcoefs]] = renderUI({
      fit_and_coefs["coefs_ui"]
    })
    
    reactive_vars[['selected_vars']] = fit_and_coefs[['coefs_names']]
    
  })
  
  button_select_them = paste0(feature_selection_prefix, "buttonSelectThem")
  observeEvent(input[[button_select_them]], {
    
    table_regression_varaibles = paste0(regression_prefix, "selectedVariables")
    output[[table_regression_varaibles]] = renderUI({
      createVariableTable(reactive_vars[['selected_vars']])
    })
    
    table_forecast_varaibles = paste0(forecast_prefix, "forecastVariables")
    output[[table_forecast_varaibles]] = renderUI({
      createForecastVariableTable(reactive_vars[['selected_vars']], input, output, session)
    })
    
  })
  
  
  
  ### Models > Regression
  regression_build_regression = paste0(regression_prefix, "buttonBuildRegression")
  observeEvent(input[[regression_build_regression]], {
    fit = doRegression(reactive_vars[['selected_vars']])
    reactive_vars[['forecast_model']] = fit
    
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

    fillVariableTable(session, fit[['regression']])
    
  })
  
  
  
  ### Models > Benchmark
  benchmark_perform_benchmark = paste0(benchmark_prefix, "buttonPerformBenchmark")
  observeEvent(input[[benchmark_perform_benchmark]], {
    
    h = input[[paste0(benchmark_prefix, "h")]]
    mean_model = input[[paste0(benchmark_prefix,"methodId|mean")]]
    naive_model = input[[paste0(benchmark_prefix,"methodId|naive")]]
    snaive_model = input[[paste0(benchmark_prefix,"methodId|snaive")]]
    drift_model = input[[paste0(benchmark_prefix,"methodId|drift")]]
    error_type = input[[paste0(benchmark_prefix,"selectErrorType")]]
    
    fit_forecast = doRegression(reactive_vars[['selected_vars']], h=h)
  
    
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
    
    results = getForecastResults(forecast_model=forecast_model, h = h,
                                  mean_model=mean_model, naive_model=naive_model,
                                  snaive_model=snaive_model, drift_model=drift_model)
    
    
    ## Plotting graphs
    output_graph_forecast = paste0(forecast_prefix, "graphForeCast")
    output[[output_graph_forecast]] = renderPlot({
      
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
    
  })
  
}

