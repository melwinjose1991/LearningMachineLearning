print("Models :: Regression :: Init")


## Globals
regression_prefix = paste0(models_prefix, "regression_")



## UI Elements
getRegressionUI = function() {
  # row 1 - variables
  text_variables = tags$h4("Variables")
  input_vars = uiOutput(paste0(regression_prefix, "input_variables"))
  row_1 = fluidRow(text_variables, input_vars)
  
  # row 2 - Buttons
  button_build_regression_id = paste0(models_prefix, "buttonBuildRegression")
  button_build_regression = actionButton(inputId = button_build_regression_id,
                                         label = "Build Regression")
  row_2 = fluidRow(button_build_regression)
  
  # row 3 - plots
  row_3 = fluidRow()
  
  # row 4 - summary
  row_4 = fluidRow()
  
  tabPanel(title = "Regression", row_1, row_2, row_3, row_4)
}



## Helper functions
doRegression = function(inputs) {
  
}