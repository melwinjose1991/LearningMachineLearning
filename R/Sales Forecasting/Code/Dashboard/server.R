library(shiny)

product = "2401"
#source("feature_selection.R")


server = function(input, output){
  
  
  features_show = paste0(features_prefix,"buttonShow")
  observeEvent(input[[features_show]],{
      
      output$all_features_box = renderUI({
        lapply( unique(meta_data$sub_category_name), 
                FUN=function(x) fluidRow(getGroupCheckBoxInput(x)) )
      })
      
    }
  )
  
  features_select = paste0(features_prefix,"buttonSelect")
  observeEvent(input[[features_select]],{
      
      x = reactiveValuesToList(input)
      print(reactiveValuesToList(input))
    
    }
  )
  
  featureSelection_LASSO = paste0(featureSelection_prefix,"buttonLASSO")
  observeEvent(input[[featureSelection_LASSO]],{
    
    output_LASSO = paste0(featureSelection_prefix, "outputLASSO")
    output[[output_LASSO]] = renderUI({
      readData(reactiveValuesToList(input))
    })
    
  })
  
  
}

#shinyApp(ui=ui, server=server)
