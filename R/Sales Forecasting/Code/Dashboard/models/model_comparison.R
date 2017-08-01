comparison_prefix = paste0(models_prefix, "comparison_")



## UI Elements
getModelComparisonUI = function(){
  
  # row - 1 : Select Models
  id = paste0(comparison_prefix, "selecteModels")
  select_models = checkboxGroupInput(id, label="Select Models", choices=MODELS, selected=MODELS)
  
  id = paste0(comparison_prefix, "plotButton")
  button_plot = actionButton(id, label="plot")
  
  row_1 = fluidRow(select_models, button_plot)
  
  
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
    
    print(df_benchmark_fit)
    
    h = no_of_benchmark_fits
    last_ym = as.yearmon(paste0(product_end_date,collapse="-"))
    valid_start_ym = format(last_ym - (h/12) + (1/12),"%Y-%m")
    valid_start_ym = as.numeric(unlist(strsplit(valid_start_ym,"-")))
    
    preview_start_ym = format(last_ym - ((2*h)/12),"%Y-%m")
    preview_start_ym = as.numeric(unlist(strsplit(preview_start_ym,"-")))
    
    t = ts(product_data, frequency=12, start=product_start_date, end=product_end_date)
    preview_window = window(t, start=preview_start_ym)
    
    models_to_use = input[[paste0(comparison_prefix, "selecteModels")]]
    id_1_1 = paste0(comparison_prefix, "plotComparison")
    output[[id_1_1]] = renderPlot({
      plot(preview_window, lwd=2)
      
      for(model in MODELS){
        if(model %in% models_to_use){
          t = ts(df_benchmark_fit[[paste0(BENCHMARK_,model)]], frequency=12, start=valid_start_ym)
          lines(t, col=MODEL_COLORS[which(MODELS==model)], lty=2)
        }
      }
      
      legend("topleft", lwd=2, lty=2, col=MODEL_COLORS, 
             legend=MODELS)
    })
    
  })
  
}