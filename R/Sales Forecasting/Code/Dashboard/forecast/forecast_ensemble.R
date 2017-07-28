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
  
  
  # row 3 - Plots
  row_3 = fluidRow()
  
  # row 4 - Table Values
  row_4 = fluidRow()
  
  tabPanel(title = "Ensemble", row_2, row_3, row_4)
  
}



## Server Functions
attachEnsembleObservers = function(input, output, session){
  
  
  
}