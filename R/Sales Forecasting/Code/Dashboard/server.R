library(shiny)

server = function(input, output, session) {
  
  reactive_vars = reactiveValues()
  
  
  
  ### Products ###
  attachProductsObservers(input, output, session, reactive_vars)
  
  
  
  ### FRED ###
  attachFREDObservers(input)
  
  
  
  ### Feature Selection ###
  #     Features
  populateFeatures(input, output, session)

  #     Feature Clusters
  attachFeatureClustersObservers(input, output, session)
  
  #     Feature Selection
  attachObservers(input, output, session, reactive_vars)
  
  
  
  ### Models ###
  #     Regression
  attachRegressionObservers(input, output, session, reactive_vars)
  
  #     Benchmark
  attachBenchmarkObservers(input, output, reactive_vars)
  
  #     Sensitivity
  attachSensitivityObservers(input, output, reactive_vars)
  
  #     TimeSeries
  attachTimeSeriesObservers(input, output)
  
  #     MARS
  attachMARSObservers(input, output, reactive_vars)
  
  #     Model Comparison
  attachModelComparisonObservers(input, output, reactive_vars)

  
    
  ### Forecast ###
  #     Linear Regression
  attachForecastObservers(input, output, reactive_vars)
  
  #     Time-Series
  attachForecastTimeSeriesObservers(input, output, reactive_vars)
  
  #     Ensemble
  attachEnsembleObservers(input, output, session, reactive_vars)
  
}

