print("Forecast :: forecast_ensemble.R :: Init")



## Globals
forecast_ensemble_prefix = paste0(forecast_tab_prefix, "forecastEnsemble_")



## UI Elements
getForecastEnsembleUI = function(){
  
  # row 2 - Options : Ensemble Techniques & Button
  id = paste0(forecast_ensemble_prefix, "models")
  checkbox_models = checkboxGroupInput(id, label="Models", choices=MODELS, 
                                       selected=MODELS, inline=TRUE)
  
  id = paste0(forecast_ensemble_prefix, "doEnsemble")
  button_do_ensemble = actionButton(id, label="Ensemble")
  
  row_2 = fluidRow(column(10, checkbox_models),
                   column(2, button_do_ensemble)
  )
  
  
  # row 3 - Model Plots
  id = paste0(forecast_ensemble_prefix, "ensemblePlot")
  plots = plotOutput(id)
  row_3 = fluidRow(plots)
  
  # row 4 - Table Values
  id = paste0(forecast_ensemble_prefix, "ensembleTable")
  table_forecast = uiOutput(id)
  row_4 = fluidRow(table_forecast)
  
  tabPanel(title = "Ensemble", row_2, row_3, row_4)
  
}



## Server Functions
getForecastEnsembleMean = function(models_to_use){
  models_to_use = paste0(models_to_use,"_forecast")
  df = df_forecast_fit[, models_to_use]
  rowMeans(df)
}

attachEnsembleObservers = function(input, output, session, reactive_vars){
  
  id_1 = paste0(forecast_ensemble_prefix, "doEnsemble")
  observeEvent(input[[id_1]], {
    
    h = no_of_forecast
    last_ym = as.yearmon(paste0(product_end_date,collapse="-"))
    models_to_use = input[[paste0(forecast_ensemble_prefix, "models")]]
    
    preview_start_ym = format(last_ym - ((1*h)/12),"%Y-%m")
    preview_start_ym = as.numeric(unlist(strsplit(preview_start_ym,"-")))
    
    forecast_start_ym = format(last_ym + (1/12),"%Y-%m")
    forecast_start_ym = as.numeric(unlist(strsplit(forecast_start_ym,"-")))
    
    forecast_end_ym = format(last_ym +(no_of_forecast/12), "%Y-%m")
    forecast_end_ym = as.numeric(unlist(strsplit(forecast_end_ym, "-")))
    
    plot_window = ts(c(product_data, rep(NA, no_of_forecast)), 
                     frequency=12, end=forecast_end_ym)
    plot_window = window(plot_window, start=preview_start_ym)
    
    # Gathering all forecasts from reactive_vars
    ensemble_forecast = getForecastEnsembleMean(models_to_use[!grepl("ENSEMBLE_", models_to_use)])
    ensemble_forecast = ts(ensemble_forecast, frequency=12, end=forecast_end_ym)
    df_forecast_fit[,DF_COL_ENSEMBLE_MEAN_FORECAST] <<- ensemble_forecast
    
    old_forecast_window = window(product_forecast_data, start=forecast_start_ym, 
                                 end=forecast_end_ym, extend=TRUE)
    df_forecast_fit[,"old_forecast"] <<- round(old_forecast_window, 2)
    
    y_min = min(df_forecast_fit[,!names(df_forecast_fit) %in% "n"], plot_window, na.rm=TRUE)
    y_max = max(df_forecast_fit[,!names(df_forecast_fit) %in% "n"], plot_window, na.rm=TRUE)
    y_range = c(y_min, y_max)
    
    
    # Ploting the ensemble forecast
    id_2 = paste0(forecast_ensemble_prefix, "ensemblePlot")
    output[[id_2]] = renderPlot({
      plot(plot_window, ylim=y_range, ylab=product_data_column )
      lines(old_forecast_window, col="orange", lty=2, lwd=3)
      
      for(model in MODELS){
        if(model %in% models_to_use){
          lines(df_forecast_fit[,paste0(model,"_forecast")], 
                col=MODEL_COLORS[which(MODELS==model)], 
                lty=ifelse(grepl("ENSEMBLE", model), 2, 3), 
                lwd=ifelse(grepl("ENSEMBLE", model), 3, 1))
        }
      } 
      
      legend("topleft", lwd=2, lty=2, col=c(MODEL_COLORS,"orange"), 
             legend=c(MODELS,"OLD_FORECAST"))
    })
    
    # Table 
    id_3 = paste0(forecast_ensemble_prefix, "ensembleTable")
    output[[id_3]] = renderUI({
      
      forecast_rows = sapply(1:no_of_forecast, function(i){
        
        row = df_forecast_fit[i,]
        tr_td = paste0("<tr><td>",i,"</td>")
        for(model in models_to_use){
            fit = round(row[[paste0(model, "_forecast")]], 2)
            tr_td = paste0(tr_td, "<td>&nbsp;",fit,"&nbsp;</td>")
        }
        tr_td = paste0(tr_td, "<td>&nbsp;",row[["old_forecast"]], "&nbsp;</td>")
        tr_td = paste0(tr_td, "</tr>")
        
      })
      forecast_rows = paste0(forecast_rows, collapse="")
      
      table_header = sapply(MODELS, function(model){
        if(model %in% models_to_use){
          paste0("<th>&nbsp;",model,"&nbsp;</th>")
        }
      })
      table_header = paste0(table_header, collapse="")
      table_header = paste0(table_header, "<th>&nbsp;OLD_FORECAST&nbsp;</th>")
      table_forecast = paste0("<table><tr><td>#</td>", table_header, "</tr>",
                              forecast_rows, "</table>")
      
      HTML(table_forecast)
    })
    
    
  })
  
}

