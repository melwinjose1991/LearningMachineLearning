forecast_tab_prefix = "forecastTab_"

source("forecast/forecast.R")



## UI Elements
forecast_tab_forecast = getForecastUI()

forecast_navbar = navlistPanel(
  well = FALSE,
  widths = c(2, 10),
  forecast_tab_forecast
)