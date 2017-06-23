library(shiny)
library(shinyjs)

if(TRUE){

product = "2401"

source("feature selection/main.R")
source("models/main.R")
source("forecast/main.R")
source("server.R")



tab_extract_data = tabPanel(title = "Extract >", "Not Implemented")

tab_feature_selection = tabPanel(title = "Feature Selection >", tags$hr(), feature_selection_navbar)

tab_models = tabPanel(title = "Models >", tags$hr(), models_navbar)

tab_forecast = tabPanel(title = "Forecast", tags$hr(), forecast_navbar)


ui = fluidPage(
  useShinyjs(),
  tabsetPanel(
    tab_extract_data,
    tab_feature_selection,
    tab_models,
    tab_forecast
  )
)

shinyApp(ui = ui, server = server)

}
