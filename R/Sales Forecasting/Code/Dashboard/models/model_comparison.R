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
getBenchmarkEnsembleMean = function(models_to_use){
  models_to_use = paste0(BENCHMARK_, models_to_use)
  df = df_benchmark_fit[, models_to_use]
  if(is.null(dim(df))){
    df
  }else{
    rowMeans(df)
  }
}

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
    
    
    ## Calculating Ensemble_Mean
    if("ENSEMBLE_MEAN" %in% models_to_use){
      models = setdiff(models_to_use, "ENSEMBLE_MEAN")
      df_benchmark_fit[,BENCHMARK_ENSEMBLE_MEAN] <<- round(getBenchmarkEnsembleMean(models), 2)
    }
    
    
    ## Ploting graph
    old_forecast_window = window(product_forecast_data, start=valid_start_ym, 
                             end=product_end_date, extend=TRUE)
    
    id_1_1 = paste0(comparison_prefix, "plotComparison")
    output[[id_1_1]] = renderPlot({
      plot(preview_window, lwd=2, ylab=product_data_column)
      lines(old_forecast_window, col="orange", lty=2, lwd=3)
      
      for(model in MODELS){
        if(model %in% models_to_use){
          t = ts(df_benchmark_fit[[paste0(BENCHMARK_,model)]], frequency=12, start=valid_start_ym)
          lines(t, col=MODEL_COLORS[which(MODELS==model)], 
                lty=ifelse(grepl("ENSEMBLE", model), 2, 3), 
                lwd=ifelse(grepl("ENSEMBLE", model), 3, 1))
        }
      }
      
      legend("topleft", lwd=2, lty=2, col=MODEL_COLORS, legend=MODELS)
    })
    
    
    ## Populating Forecast
    df_benchmark_fit[,"actuals"] <<- round(tail(product_data, n=h), 2)
    df_benchmark_fit[,"best_model"] <<- rep("-", no_of_forecast)
    df_benchmark_fit[,"worst_model"] <<- rep("-", no_of_forecast)
    df_benchmark_fit[,"old_forecast"] <<- round(old_forecast_window, 2)
    df_benchmark_fit[,"old_forecast_error"] <<- round(old_forecast_window - df_benchmark_fit[,"actuals"], 2)
    
    for(i in 1:no_of_benchmark_fits){
      row = df_benchmark_fit[i,]
      min_error = .Machine$integer.max
      max_error = 0
      best_model = "-"
      worst_model = "-"
      
      for(model in MODELS){
        if(model %in% models_to_use){
          fit = row[[paste0(BENCHMARK_,model)]]
          actual = row[["actuals"]]
          error = round(abs(fit-actual), 2)
          if(error < min_error){
            min_error = error
            best_model = model
          }
          if(error > max_error){
            max_error = error
            worst_model = model
          }
        }
      }
      
      df_benchmark_fit[i,"best_model"] <<- best_model
      df_benchmark_fit[i,"worst_model"] <<- worst_model
    }
    
    for(model in MODELS){
      if(model %in% models_to_use){
        error = df_benchmark_fit[,"actuals"] - df_benchmark_fit[,paste0(BENCHMARK_,model)]
        df_benchmark_fit[,paste0(BENCHMARK_,model,"_ERROR")] <<- round(error, 2)
      }
    }    

    id_1_2 = paste0(comparison_prefix, "forecastSummary")    
    output[[id_1_2]] = renderUI({
      
      benchmark_rows = sapply(1:no_of_benchmark_fits, function(i){
        row = df_benchmark_fit[i,]
        tr_td = paste0("<tr><td>",i,"</td>")
        for(model in MODELS){
          if(model %in% models_to_use){
            fit = row[[paste0(BENCHMARK_,model)]]
            error = row[[paste0(BENCHMARK_,model,"_ERROR")]]
            if(model==row[["best_model"]]){
              tr_td = paste0(tr_td, 
                             "<td style='background-color:#87e595'>&nbsp;",fit,"&nbsp;</td>",
                             "<td style='background-color:#87e595'>&nbsp;",error,"&nbsp;</td>")
              
            }else if(model==row[["worst_model"]]){
              tr_td = paste0(tr_td, 
                             "<td style='background-color:#ff793f'>&nbsp;",fit,"&nbsp;</td>",
                             "<td style='background-color:#ff793f'>&nbsp;",error,"&nbsp;</td>")
            }else{
              tr_td = paste0(tr_td, 
                             "<td>&nbsp;",fit,"&nbsp;</td>",
                             "<td>&nbsp;",error,"&nbsp;</td>")
            }
          }
        }
        
        old_forecast = paste0("<td>", row[["old_forecast"]], "</td>",
                              "<td>", row[["old_forecast_error"]], "</td>")
        
        tr_td = paste0(tr_td, old_forecast, "</tr>")
      })
      benchmark_rows = paste0(benchmark_rows, collapse="")
      
      benchmark_error_summary = paste0("<tr><td></td>")
      for(model in MODELS){
        if(model %in% models_to_use){
          benchmark_error_summary = paste0(benchmark_error_summary, "<td></td>")
          error = mean(abs(df_benchmark_fit[,paste0(BENCHMARK_,model,"_ERROR")]))
          error = round(error, 2)
          error = paste0("<td>", error, "</td>")
          benchmark_error_summary =paste0(benchmark_error_summary, error)
        }
      }
      old_forecast_mae = round( mean(abs(df_benchmark_fit[,"old_forecast_error"]), na.rm=TRUE), 2 )
      benchmark_error_summary = paste0(benchmark_error_summary, "<td></td>",
                                       "<td>", old_forecast_mae, "</td>")
      benchmark_error_summary = paste0(benchmark_error_summary, "</tr>")
                                       
      
      table_header = sapply(MODELS, function(model){
        if(model %in% models_to_use){
          paste0("<th>&nbsp;",model,"&nbsp;</th>",
                 "<th>&nbsp;error&nbsp;</th>")
        }
      })
      table_header = paste0(table_header, collapse="")
      table_header = paste0(table_header,"<th>OLD_FORECAST</th>",
                            "<th>&nbsp;error&nbsp;</th>")
      
      
      table_forecast = paste0("<table><tr><td>#</td>", table_header, "</tr>",
                            benchmark_rows, benchmark_error_summary, "</table>")
      
      HTML(table_forecast)
    })
    
    
  })
  
}