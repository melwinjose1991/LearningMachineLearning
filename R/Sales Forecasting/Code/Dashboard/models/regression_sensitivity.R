print("Models :: LRegression Sensitivity:: Init")



## Globals
sensitivity_prefix = paste0(models_prefix, "regressionSensitivity_")



## UI Elements
getRegressionSensitivityUI = function(){
  
  # row_0 - Show Button
  id = paste0(sensitivity_prefix, "show")
  button_show = actionButton(id, "Show")
  row_0 = fluidRow(button_show)
  
  # row_1 - graphs
  id = paste0(sensitivity_prefix, "plot")
  graph_benchmark = plotOutput(id)
  row_1 = fluidRow(graph_benchmark)
  
  # row_2 - sensitivity analysis
  id = paste0(sensitivity_prefix, "sensitivityTable")
  table_sensitivity = uiOutput(id)
  
  id = paste0(benchmark_prefix, "featureValues")
  table_feature_values = uiOutput(id)
  
  row_2 = fluidRow(column(8, table_sensitivity), 
                   column(4, table_feature_values)
  )
  
  tabPanel(title = "LRegression Sensitivity", row_0, row_1, row_2)
  
}



## Server Functions
attachSensitivityObservers = function(input, output, reactive_vars){
  
  id_show_button = paste0(sensitivity_prefix, "show")
  observeEvent(input[[id_show_button]],{
    
    h = 12
    df_ = data.frame(t=1:(2*h))
    
    id_output_sensitivity_plot = paste0(sensitivity_prefix, "plot")
    output[[id_output_sensitivity_plot]] = renderPlot({
      
      last_ym = as.yearmon(paste0(product_end_date, collapse="-"))
      valid_start_ym = format(last_ym - (h/12) + (1/12),"%Y-%m")
      valid_start_ym = as.numeric(unlist(strsplit(valid_start_ym,"-")))
      
      preview_start_ym = format(last_ym - ((2*h)/12),"%Y-%m")
      preview_start_ym = as.numeric(unlist(strsplit(preview_start_ym,"-")))
      
      t = ts(product_data, frequency=12, start=product_start_date, end=product_end_date)
      preview_window = window(t, start=preview_start_ym)
      # df_[,"actuals"] = preview_window
      
      forecast = ts(df_benchmark_fit[,BENCHMARK_LREGRESSION], frequency=12,
                    start=valid_start_ym)
      
      plot(preview_window, ylab=product_data_column)
      lines(forecast, col="red", lwd=2, lty=2)
    
    })
    
    # browser()
    model = reactive_vars[[SENSITIVITY_MODELS_LINEAR_REGRESSION]]
    # df_x_y = as.data.frame(tail(model$model, 2*h))
    
    df_coef = data.frame(feature=names(model$coefficients),
                         coef=model$coefficients)
    rownames(df_coef) = c()
    intercept = df_coef[1,2]
    df_coef = df_coef[2:dim(df_coef)[1],]
    df_coef[,"percent_change"] = round((df_coef$coef*100)/intercept, 2)
    
    avg_prediction = paste0("<b>Average Prediction : $", round(intercept, 3), "</b><hr/>")
    
    id_sentivity_table = paste0(sensitivity_prefix, "sensitivityTable")
    output[[id_sentivity_table]] = renderUI({
      
      rows = sapply(1:nrow(df_coef), function(i){
        
        feature_id = as.character(df_coef[i,"feature"])
        feature_name = as.character(meta_data[meta_data$series_id==feature_id, "title"])
        if(length(feature_name)>0){
          td_feature = paste0("<td>", feature_name, "&nbsp;</td>")
        }else{
          td_feature = paste0("<td>", feature_id, "&nbsp;</td>")
        }
        
        td_change = paste0("<td>&nbsp;", df_coef[i,"percent_change"], "</td>")
        row = paste0("<tr>",td_feature,td_change,"</tr>")
      })
      
      rows = paste0(rows, collapse="")
      
      table_header = paste0("<tr><td><b>Feature</b></td><td><b>%</b></td></tr>")
      table = paste0("<table>", table_header, rows, "</table>")
      
      HTML(avg_prediction, table)
      
    })
    
    
  })
  
}


