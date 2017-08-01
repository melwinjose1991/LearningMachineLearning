comparison_prefix = paste0(models_prefix, "comparison_")



## UI Elements
getModelComparisonUI = function(){
  
  # row - 1 : Select Models
  id = paste0(comparison_prefix, "selecteModels")
  select_models = checkboxGroupInput(id, label="Select Models", inline=TRUE,
                                     choices=MODELS, selected=MODELS)
  
  id = paste0(comparison_prefix, "plotButton")
  button_plot = actionButton(id, label="Compare")
  
  row_1 = fluidRow(column(10, select_models), 
                   column(2, button_plot))
  
  
  # row - 2 : Plot
  id = paste0(comparison_prefix, "plotComparison")
  plot_comparison = plotOutput(id)
  
  row_2 = fluidRow(plot_comparison)
  
  # row -3 : Forecast Value
  id = paste0(comparison_prefix, "forecastSummary")
  ui_summary = uiOutput(id)
  
  row_3 = fluidRow(ui_summary)
  
  tabPanel(title = "Model Comparison", row_1, row_2, row_3)
}



## Server Functions
attachModelComparisonObservers = function(input, output, reactive_vars){
  
  id_1 = paste0(comparison_prefix, "plotButton")
  observeEvent(input[[id_1]], {
    
    h = no_of_benchmark_fits
    models_to_use = input[[paste0(comparison_prefix, "selecteModels")]]
    
    last_ym = as.yearmon(paste0(product_end_date,collapse="-"))
    valid_start_ym = format(last_ym - (h/12) + (1/12),"%Y-%m")
    valid_start_ym = as.numeric(unlist(strsplit(valid_start_ym,"-")))
    
    preview_start_ym = format(last_ym - ((2*h)/12),"%Y-%m")
    preview_start_ym = as.numeric(unlist(strsplit(preview_start_ym,"-")))
    
    t = ts(product_data, frequency=12, start=product_start_date, end=product_end_date)
    preview_window = window(t, start=preview_start_ym)
    
    ## Ploting graph
    id_1_1 = paste0(comparison_prefix, "plotComparison")
    output[[id_1_1]] = renderPlot({
      plot(preview_window, lwd=2)
      
      for(model in MODELS){
        if(model %in% models_to_use){
          t = ts(df_benchmark_fit[[paste0(BENCHMARK_,model)]], frequency=12, start=valid_start_ym)
          lines(t, col=MODEL_COLORS[which(MODELS==model)], lty=2, lwd=2)
        }
      }
      
      legend("topleft", lwd=2, lty=2, col=MODEL_COLORS, 
             legend=MODELS)
    })
    
    ## Populating Forecast
    df_benchmark_fit[,"actuals"] <<- round(tail(product_data, n=h), 2)
    df_benchmark_fit[,"best_model"] <<- rep("-", no_of_forecast)
    for(i in 1:no_of_benchmark_fits){
      row = df_benchmark_fit[i,]
      min_error = .Machine$integer.max
      best_model = "-"
      for(model in MODELS){
        if(model %in% models_to_use){
          fit = row[[paste0(BENCHMARK_,model)]]
          actual = row[["actuals"]]
          error = round(abs(fit-actual), 2)
          if(error < min_error){
            min_error = error
            best_model = model
          }
        }
      }
      df_benchmark_fit[i,"best_model"] <<- best_model
    }
        
    id_1_2 = paste0(comparison_prefix, "forecastSummary")    
    output[[id_1_2]] = renderUI({
      
      benchmark_rows = sapply(1:no_of_benchmark_fits, function(i){
        row = df_benchmark_fit[i,]
        tr_td = paste0("<tr><td>",i,"</td>")
        for(model in MODELS){
          if(model %in% models_to_use){
            fit = row[[paste0(BENCHMARK_,model)]]
            actual = row[["actuals"]]
            error = round(abs(fit-actual), 2)
            if(model==row[["best_model"]]){
              tr_td = paste0(tr_td, 
                             "<td style='background-color:#87e595'>&nbsp;",fit,"&nbsp;</td>",
                             "<td style='background-color:#87e595'>&nbsp;",error,"&nbsp;</td>")
            }else{
              tr_td = paste0(tr_td, 
                             "<td>&nbsp;",fit,"&nbsp;</td>",
                             "<td>&nbsp;",error,"&nbsp;</td>")
            }
          }
        }
        tr_td = paste0(tr_td, "</tr>")
      })
      benchmark_rows = paste0(benchmark_rows, collapse="")
      
      table_header = sapply(MODELS, function(model){
        if(model %in% models_to_use){
          paste0("<th>&nbsp;",model,"-fit&nbsp;</th>",
                 "<th>&nbsp;",model,"-error&nbsp;</th>")
        }
      })
      table_header = paste0(table_header, collapse="")
      table_forecast = paste0("<table><tr><td>#</td>", table_header, "</tr>",
                            benchmark_rows, "</table>")
      
      HTML(table_forecast)
    })
    
    
  })
  
}