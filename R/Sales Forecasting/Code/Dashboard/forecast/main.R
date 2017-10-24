print("Forecast :: main.R :: Init")



# Globals & Includes
forecast_tab_prefix = "forecastTab_"
no_of_forecast = 4

source("forecast/forecast_lregression.R")
source("forecast/forecast_timeseries.R")
source("forecast/forecast_ensemble.R")



## UI Elements
forecast_tab_lregression = getForecastLRegressionUI()
forecast_tab_timeseries = getForecastTimeSeriesUI()
forecast_tab_ensemble = getForecastEnsembleUI()

forecast_navbar = navlistPanel(
  well = FALSE,
  widths = c(2, 10),
  forecast_tab_lregression,
  forecast_tab_timeseries,
  forecast_tab_ensemble
)