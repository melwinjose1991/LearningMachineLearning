library(shiny)

source("feature_selection.R")

product = "2401"

server = function(input, output){
  
  observeEvent(input$all_features_show,{
    
      output$all_features_box = renderUI({
        lapply( unique(meta_data$sub_category_name), 
                FUN=function(x) fluidRow(getGroupCheckBoxInput(x)) )
      })
      
    }
  )
  
  observeEvent(input$all_features_select,{
    
    print(reactiveValuesToList(input))
    
    }
  )
  
  
}

shinyApp(ui=ui, server=server)
