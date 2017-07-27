library(shiny)
library(shinyjs)
library(ggplot2)

if(TRUE){

source("const.R")
  
source("products/main.R")
source("extract data/main.R")
source("feature selection/main.R")
source("models/main.R")
source("forecast/main.R")

source("server.R")

  
tab_products = tabPanel(title = "Products >", tags$hr(), products_navbar)

tab_extract_data = tabPanel(title = "Extract >", tags$hr(), extract_navbar)

tab_feature_selection = tabPanel(title = "Feature Selection >", tags$hr(), feature_selection_navbar)

tab_models = tabPanel(title = "Models >", tags$hr(), models_navbar)

tab_forecast = tabPanel(title = "Forecast", tags$hr(), forecast_navbar)


ui = fluidPage(
  useShinyjs(),
  tabsetPanel(
    tab_products,
    tab_extract_data,
    tab_feature_selection,
    tab_models,
    tab_forecast
  )
)

shinyApp(ui = ui, server = server)

}
