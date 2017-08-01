print("Models :: Regression-MARS :: Init")
library(earth)


## Globals
mars_prefix = paste0(models_prefix, "mars_")



## UI Elements
getMARSUI = function() {

  # row - 0 : Options
  id = paste0(mars_prefix, "pmethod")
  select_pmethod = selectInput(id, label="Prune Method", 
                               choices=c("cv","forward", "exhaustive"),
                               selected="exhaustive")
  
  id = paste0(mars_prefix, "nfolds")
  numeric_nfolds = numericInput(id, label="nfolds", value=13, 
                                min=2, max=length(product_data))
  
  id = paste0(mars_prefix, "doMARS")
  button_do_mars = actionButton(id, label="MARS")
  
  row_0 = fluidRow(select_pmethod, numeric_nfolds, button_do_mars)
  
  
  # row - 1 : Plots
  id = paste0(mars_prefix, "modelSelectionPlot")
  plot_model_selection = plotOutput(id)
  
  id = paste0(mars_prefix, "insamplePredictionPlot")
  plot_insample_prediction = plotOutput(id)
  
  row_1 = fluidRow(column(6, plot_model_selection),
                   column(6, plot_insample_prediction))
  
  
  # row - 2 : Model Summary
  id = paste0(mars_prefix, "selectedModel")
  html_selected_model = uiOutput(id)
  
  id = paste0(mars_prefix, "insamplePredictionValues")
  html_prediction_values = uiOutput(id)
  
  row_2 = fluidRow(column(6, html_selected_model),
                   column(6, html_prediction_values))

    
  tabPanel(title = "MARS", row_0,  row_1, row_2)
}



## Server Functions
attachMARSObservers = function(input, output, reactive_vars){
  
  id_1 = paste0(mars_prefix, "doMARS")
  observeEvent(input[[id_1]],{
    
    h = 12
    nfolds = as.numeric(input[[paste0(mars_prefix, "nfolds")]])
    pmethod = input[[paste0(mars_prefix, "pmethod")]]
    
    print(nfolds)
    print(pmethod)
    
    data = readData(input)
    form = as.formula(paste0(product_data_column, "~."))
    mars = earth(form, data=data, pmethod=pmethod, 
                 nfold=nfolds, nprune=20, degree=1,
                 minspan=3, endspan=3)
    
    id_1_1 = paste0(mars_prefix, "modelSelectionPlot")
    output[[id_1_1]] = renderPlot({
      plot(mars, which=1)
    })
    
    id_1_2 = paste0(mars_prefix, "insamplePredictionPlot")
    output[[id_1_2]] = renderPlot({
      
      last_ym = as.yearmon(paste0(product_end_date, collapse="-"))
      
      valid_start_ym = format(last_ym - (h/12) + (1/12),"%Y-%m")
      valid_start_ym = as.numeric(unlist(strsplit(valid_start_ym,"-")))
      
      preview_start_ym = as.yearmon(paste0(valid_start_ym, collapse="-"))     
      preview_start_ym = format(preview_start_ym - (h/12),"%Y-%m")
      preview_start_ym = as.numeric(unlist(strsplit(preview_start_ym,"-")))
      
      t = ts(product_data, frequency=12, start=product_start_date, end=product_end_date)
      win = window(t, start=preview_start_ym)
      pred = tail(mars$fitted.values,n=h)
      
      y_min = min(win, pred)
      y_max = max(win, pred)
      
      plot(win, ylim=c(y_min, y_max))
      lines(ts(pred, frequency=12, start=valid_start_ym), 
            col="red", lwd=2, lty=2)
    })
    
    id_1_3 = paste0(mars_prefix, "selectedModel")
    df = as.data.frame(mars$coefficients)
    df[,"var"] = row.names(mars$coefficients)
    output[[id_1_3]] = renderUI({
      table_header = paste0("<table><tr>",
                            "<th>#</th>",
                            "<th>Variable</th>",
                            "<th>Coef</th>",
                            "</tr>")
      rows = sapply(1:nrow(df), function(i){
        paste0("<tr><td>&nbsp;",i,"&nbsp;</td>",
               "<td>&nbsp;",df[i, "var"],"&nbsp;</td>",
               "<td>&nbsp;",round(df[i, product_data_column],2),"&nbsp;</td></tr>")
      })
      rows = paste0(rows, collapse="")
      HTML(paste0(table_header, rows, "</table>"))
      
    })
    
    id_1_4 = paste0(mars_prefix, "insamplePredictionValues")
    df_1 = data.frame(n=1:h)
    df_1[,"fit"] = tail(mars$fitted.values, n=h)
    df_1[,"error"] = tail(mars$residuals, n=h)
    df_benchmark_fit[,BENCHMARK_MARS] <<- round(df_1[,"fit"], 2)
    
    output[[id_1_4]] = renderUI({
      table_header = paste0("<table><tr>",
                            "<th>#</th>",
                            "<th>Fit</th>",
                            "<th>Error</th>",
                            "</tr>")
      rows = sapply(1:nrow(df_1), function(i){
        paste0("<tr><td>&nbsp;",i,"&nbsp;</td>",
               "<td>&nbsp;",round(df_1[i, "fit"],2),"&nbsp;</td>",
               "<td>&nbsp;",round(df_1[i, "error"],2),"&nbsp;</td></tr>")
      })
      rows = paste0(rows, collapse="")
      table_fit_error = paste0(table_header, rows, "</table>")
      
      pred_mae = paste0("<hr/><b>MAE for last ",h," points</b> : ",
                        mean(abs(tail(mars$residuals, n=h))))
      
      full_mae = paste0("<hr/><b>MAE for ",product_data_obeservations," points</b> : ",
                   mean(abs(mars$residuals)))
      
      to_print = paste0(table_fit_error, pred_mae, full_mae)

      HTML(to_print)
            
    })
    
    reactive_vars[[MODEL_MARS]] = mars
    
  })
  
}