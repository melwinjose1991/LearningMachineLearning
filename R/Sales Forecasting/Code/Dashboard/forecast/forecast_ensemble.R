print("Forecast :: forecast_ensemble.R :: Init")



## Globals
forecast_ensemble_prefix = paste0(forecast_tab_prefix, "forecastEnsemble_")



## UI Elements
getForecastEnsembleUI = function(){
  
  # row 2 - Options : Ensemble Techniques & Button
  id = paste0(forecast_ensemble_prefix, "models")
  checkbox_models = checkboxGroupInput(id, label="Models", choices=MODELS, 
                                       selected=MODELS, inline=TRUE)
  
  id = paste0(forecast_ensemble_prefix, "ensembleTechnique")
  select_technique = selectInput(id, label="Technique", choices=ENSEMBLE_TECHNIQUES, 
                                 selected=ENSEMBLE_TECHNIQUES[1])
  
  id = paste0(forecast_ensemble_prefix, "doEnsemble")
  button_do_ensemble = actionButton(id, label="Ensemble")
  
  row_2 = fluidRow(column(4, checkbox_models),
                   column(2, select_technique), 
                   column(2, button_do_ensemble)
                   )
  
  
  # row 3 - Model Plots
  id = paste0(forecast_ensemble_prefix, "modelPlot")
  plots = plotOutput(id)
  row_3 = fluidRow(plots)
  
  # row 4 - Table Values
  row_4 = fluidRow()
  
  tabPanel(title = "Ensemble", row_2, row_3, row_4)
  
}



## Server Functions
getEnsembleMean = function(df_forecast){
  df = df_forecast[,grepl("_forecast", names(df_forecast))]
  rowMeans(df)
}

attachEnsembleObservers = function(input, output, session, reactive_vars){
  
  id_1 = paste0(forecast_ensemble_prefix, "doEnsemble")
  observeEvent(input[[id_1]], {
    
    h = no_of_forecast
    last_ym = as.yearmon(paste0(product_end_date,collapse="-"))
    
    preview_start_ym = format(last_ym - ((1*h)/12),"%Y-%m")
    preview_start_ym = as.numeric(unlist(strsplit(preview_start_ym,"-")))
    
    forecast_end_ym = format(last_ym +(no_of_forecast/12), "%Y-%m")
    forecast_end_ym = as.numeric(unlist(strsplit(forecast_end_ym, "-")))
    
    plot_window = ts(c(product_data, rep(NA, no_of_forecast)), 
                     frequency=12, end=forecast_end_ym)
    plot_window = window(plot_window, start=preview_start_ym)
    
    # Gathering all forecasts from reactive_vars
    df_forecast = data.frame(n=1:no_of_forecast)
    for(model in MODELS){
      df = reactive_vars[[paste0(FORECAST_,model)]]
      df = df[,!names(df) %in% "n"]
      df_forecast = cbind(df_forecast, df)
    }
    ensemble_forecast = getEnsembleMean(df_forecast)
    ensemble_forecast = ts(ensemble_forecast, frequency=12, 
                           end=forecast_end_ym)
    
    y_min = min(df[,!names(df) %in% "n"])
    y_max = max(df[,!names(df) %in% "n"])
    y_range = c(y_min, y_max)
    
    id_2 = paste0(forecast_ensemble_prefix, "modelPlot")
    output[[id_2]] = renderPlot({
      plot(plot_window, ylim=y_range, ylab=product_data_column )
      for(index in 1:length(MODELS)){
        lines(df_forecast[,paste0(MODELS[index],"_forecast")], 
              col=MODEL_COLORS[index], lwd=3, lty=2)
      }  
      lines(ensemble_forecast, col="green", lwd=3, lty=2)
      
      legend("topleft", lwd=2, lty=2, col=c(MODEL_COLORS,"green"), 
             legend=c(MODELS,"ENSEMBLE"))
    })
    
    
  })
  
}

