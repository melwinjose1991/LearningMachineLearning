library(shiny)

product = "2401"

server = function(input, output) {
  features_show = paste0(features_prefix, "buttonShow")
  observeEvent(input[[features_show]], {
    output$all_features_box = renderUI({
      lapply(
        unique(meta_data$sub_category_name),
        FUN = function(x)
          fluidRow(getGroupCheckBoxInput(x))
      )
    })
    
  })
  
  features_select = paste0(features_prefix, "buttonSelect")
  observeEvent(input[[features_select]], {
    x = reactiveValuesToList(input)
    print(reactiveValuesToList(input))
    
  })
  
  featureSelection_LASSO = paste0(featureSelection_prefix, "buttonLASSO")
  observeEvent(input[[featureSelection_LASSO]], {
    fit_and_coefs = readData(reactiveValuesToList(input))
    
    output_LASSOgraph = paste0(featureSelection_prefix, "outputLASSOGraph")
    output[[output_LASSOgraph]] = renderPlot({
      plot(fit_and_coefs[["fit"]])
    })
    
    output_LASSOcoefs = paste0(featureSelection_prefix, "outputLASSOCoefs")
    output[[output_LASSOcoefs]] = renderUI({
      fit_and_coefs["coefs"]
    })
    
  })
  
  
}

#shinyApp(ui=ui, server=server)
