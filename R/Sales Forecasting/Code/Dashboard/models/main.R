models_prefix = "models_"

source("models/regression.R")



## UI Elements
models_tab_regression = getRegressionUI()
models_tab_timeSeries = tabPanel(title = "Time Series", "...")
models_tab_benchmark = tabPanel(title = "Benchmark", "...")

models_navbar = navlistPanel(
  well = FALSE,
  widths = c(2, 8),
  models_tab_regression,
  models_tab_timeSeries,
  models_tab_benchmark
)
