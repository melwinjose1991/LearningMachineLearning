print("Forecast :: forecast_MARS.R :: Init")



## Globals
forecast_MARS_prefix = paste0(forecast_tab_prefix, "forecastMARS_")



## UI Elements
getForecastMARSUI = function(){
  
  # row - 0 : Options
  id = paste0(forecast_MARS_prefix, "pmethod")
  select_pmethod = selectInput(id, label="Prune Method", 
                               choices=c("cv","forward", "exhaustive"),
                               selected="exhaustive")
  
  id = paste0(forecast_MARS_prefix, "nfolds")
  numeric_nfolds = numericInput(id, label="nfolds", value="13", 
                                min=2, max=length(product_data))
  
  id = paste0(forecast_MARS_prefix, "doMARS")
  button_do_mars = actionButton(id, label="MARS")
  
  row_0 = fluidRow(column(3, select_pmethod), 
                   column(3, numeric_nfolds), 
                   column(3, button_do_mars)
  )
  
  
  # row - 1 : Plots
  id = paste0(mars_prefix, "insamplePredictionPlot")
  plot_insample_prediction = plotOutput(id)
  
  row_1 = fluidRow(plot_insample_prediction)
  
  
  # row - 2 : Model Summary
  id = paste0(forecast_MARS_prefix, "selectedModel")
  html_selected_model = uiOutput(id)
  
  id = paste0(forecast_MARS_prefix, "insamplePredictionValues")
  html_prediction_values = uiOutput(id)
  
  id = paste0(forecast_MARS_prefix, "insamplePredictionError")
  html_prediction_error = uiOutput(id)
  
  row_2 = fluidRow(column(5, html_selected_model),
                   column(4, html_prediction_values),
                   column(3, html_prediction_error)
  )
  
  
  tabPanel(title = "MARS", row_0,  row_1, row_2)
}