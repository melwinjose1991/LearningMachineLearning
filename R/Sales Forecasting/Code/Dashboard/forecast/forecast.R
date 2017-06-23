## Globals
forecast_prefix = paste0(forecast_tab_prefix, "forecast_")



## UI Elements
getForecastUI = function(){
  
  # row 1 - variables
  text_variables = tags$h4("Variables")
  input_vars = uiOutput(paste0(forecast_prefix, "forecastVariables"))
  row_1 = fluidRow(text_variables, input_vars)
  
  # row 2 - Buttons
  button_forecast_id = paste0(forecast_prefix, "buttonForecast")
  button_forecast = actionButton(inputId = button_forecast_id,
                                         label = "Forecast")
  row_2 = fluidRow(button_forecast)
  
  # row 3 - plots
  id = paste0(forecast_prefix, "graphForeCast")
  output_graph_forecast = plotOutput(id)
  row_3 = fluidRow(output_graph_forecast)
  
  # row 4 - ???
  row_4 = fluidRow()
  
  # 
  tabPanel(title = "Forecast", row_1, row_2, row_3, row_4)
  
}



## Server Functions
createForecastVariableTable = function(variables){
  
  vars_titles = list(
    fluidRow(
      column(width=2, tags$h5("Variable")),
      column(width=2, tags$h5("Value")),
      column(width=2, tags$h5("Method")),
      column(width=2, tags$h5("Parameters")),
      column(width=2, tags$h5("Select"))
    )
  )
  
  vars_rows = lapply(variables, function(var){
    
    var_id = paste0(regression_prefix, "varId|", var)
    
    id = paste0(forecast_prefix, "varId|", var, "|value")
    input_text_var_value = textInput(id, label=NULL, placeholder="Enter or Select")
    
    id = paste0(forecast_prefix, "varId|", var, "|method")
    input_select_method = selectInput(id, label=NULL, 
                                  c("User Entered" = "user",
                                    "Use Mean" = "mean",
                                    "Use Last Value" = "last_value",
                                    "Time-Series" = "time_series"
                                  ))
    
    id = paste0(forecast_prefix, "varId|", var, "|parameters")
    input_method_params = tags$div(id=id, "params")
    
    id = paste0(regression_prefix, "varId|", var, "use")
    input_button_use_value = actionButton(id, label="Use")
    
    fluidRow(
      column(width=2, tags$h5(var)),
      column(width=2, input_text_var_value),
      column(width=2, input_select_method),
      column(width=2, input_method_params),
      column(width=2, input_button_use_value)
    )
    
  })
  
  list(vars_titles, vars_rows)
  
}