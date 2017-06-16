library(shiny)

product = "2401"

source("feature selection/main.R")
source("server.R")



tab_extract_data = tabPanel(title = "Extract >", "Not Implemented")

tab_feature_selection = tabPanel(title = "Feature Selection >", tags$hr(), feature_selection_navbar)

tab_build = tabPanel(title = "Build >", "...")

tab_benchmark = tabPanel(title = "Benchmark >", "...")

tab_forecast = tabPanel(title = "Forecast", "...")


ui = fluidPage(
  tabsetPanel(
    tab_extract_data,
    tab_feature_selection,
    tab_build,
    tab_benchmark,
    tab_forecast
  )
)

shinyApp(ui = ui, server = server)
