library(shiny)

product = "2401"

server = function(input, output, session) {
  
  
  ### Feature Selection > Features
  output$all_features_box = renderUI({
    lapply(
      unique(meta_data$sub_category_name),
      FUN = function(x)
        fluidRow(getGroupCheckBoxInput(x))
    )
  })
  
  groups = unique(meta_data$sub_category_name)
  lapply(groups, function(group) {
    select_all_id = paste0(features_prefix, "selectAllId|", group)
    show_button_id = paste0(features_prefix, "showButtonId|", group)
    
    observeEvent(input[[select_all_id]], {
      all_features = vector('character')
      for (key in names(input)) {
        if (grepl(paste0("_fid|", group), key)) {
          all_features = c(all_features, key)
        }
      }
      
      if (input[[select_all_id]]) {
        lapply(all_features, function(fid) {
          updateCheckboxInput(session, fid, value = TRUE)
        })
      } else{
        lapply(all_features, function(fid) {
          updateCheckboxInput(session, fid, value = FALSE)
        })
      }
      
    })
    
    observeEvent(input[[show_button_id]], {
      print(paste0("button : ", input[[show_button_id]]))
      if (input[[show_button_id]] %% 2 == 0) {
        showElement(paste0(features_prefix, "divId_", group))
        updateActionButton(session, show_button_id, label = "Hide")
      } else{
        hideElement(paste0(features_prefix, "divId_", group))
        updateActionButton(session, show_button_id, label = "Show")
      }
    })
    
  })
  
  
  
  ### Feature Selection > Feature Selection
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

