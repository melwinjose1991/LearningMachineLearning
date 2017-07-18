models_prefix = "models_"

source("models/regression.R")
source("models/regression_benchmark.R")
source("models/timeseries.R")
source("models/model_comparison.R")


## UI Elements
models_tab_regression = getRegressionUI()
models_tab_regression_benchmark = getRegressionBenchmarkUI()
models_tab_timeSeries = getTimeSeriesUI()
models_tab_comparison = getModelComparisonUI()

models_navbar = navlistPanel(
  well = FALSE,
  widths = c(2, 8),
  models_tab_regression,
  models_tab_regression_benchmark,
  models_tab_timeSeries,
  models_tab_comparison
)
