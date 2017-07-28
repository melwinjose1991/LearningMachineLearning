library(shiny)

server = function(input, output, session) {
  
  reactive_vars = reactiveValues()
  
  ### Products
  attachProductsObservers(input, output, session, reactive_vars)
  
  ### FRED
  attachFREDObservers(input)
  
  
  ### Feature Selection > Features
  populateFeatures(input, output, session)

  ## Feature Selection > Feature Clusters
  attachFeatureClustersObservers(input, output, session)
  
  ### Feature Selection > Feature Selection
  attachObservers(input, output, session, reactive_vars)
  
  
  ### Models > Regression
  attachRegressionObservers(input, output, session, reactive_vars)
  
  ### Models > TimeSeries
  attachTimeSeriesObservers(input, output)
  
  ### Models > Benchmark
  attachBenchmarkObservers(input, output, reactive_vars)

    
  ### Forecast > Linear Regression
  attachForecastObservers(input, output, reactive_vars)
  
  ## Forecast > Time-Series
  attachForecastTimeSeriesObservers(input, output, session)
  
}

