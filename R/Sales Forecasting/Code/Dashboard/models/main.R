models_prefix = "models_"
models_forecast_h = no_of_benchmark_fits



source("models/regression.R")
source("models/regression_benchmark.R")
source("models/regression_sensitivity.R")
source("models/regression_MARS.R")
source("models/timeseries.R")
source("models/model_comparison.R")



## UI Elements
models_tab_regression = getRegressionUI()
models_tab_regression_benchmark = getRegressionBenchmarkUI()
models_tab_regression_sensitivity = getRegressionSensitivityUI()
models_tab_regression_MARS = getMARSUI()
models_tab_timeSeries = getTimeSeriesUI()
models_tab_comparison = getModelComparisonUI()

models_navbar = navlistPanel(
  well = FALSE,
  widths = c(2, 8),
  models_tab_regression,
  models_tab_regression_benchmark,
  models_tab_regression_sensitivity,
  models_tab_regression_MARS,
  models_tab_timeSeries,
  models_tab_comparison
)
