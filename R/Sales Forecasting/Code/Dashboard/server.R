library(shiny)

product = "2401"

server = function(input, output, session) {
  
  reactive_vars = reactiveValues()
  
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
    select_all_id = paste0(all_features_prefix, "selectAllId|", group)
    show_button_id = paste0(all_features_prefix, "showButtonId|", group)
    
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
        showElement(paste0(all_features_prefix, "divId_", group))
        updateActionButton(session, show_button_id, label = "Hide")
      } else{
        hideElement(paste0(all_features_prefix, "divId_", group))
        updateActionButton(session, show_button_id, label = "Show")
      }
    })
    
  })
  
  
  
  ### Feature Selection > Feature Selection
  featureSelection_LASSO = paste0(feature_selection_prefix, "buttonLASSO")
  observeEvent(input[[featureSelection_LASSO]], {
    fit_and_coefs = readData(reactiveValuesToList(input))
    
    output_LASSOgraph = paste0(feature_selection_prefix, "outputLASSOGraph")
    output[[output_LASSOgraph]] = renderPlot({
      plot(fit_and_coefs[["fit"]])
    })
    
    output_LASSOcoefs = paste0(feature_selection_prefix, "outputLASSOCoefs")
    output[[output_LASSOcoefs]] = renderUI({
      fit_and_coefs["coefs_ui"]
    })
    
    reactive_vars[['selected_vars']] = fit_and_coefs[['coefs_names']]
    
  })
  
  button_select_them = paste0(feature_selection_prefix, "buttonSelectThem")
  observeEvent(input[[button_select_them]], {
    
    table_varaibles = paste0(regression_prefix, "selectedVariables")
    output[[table_varaibles]] = renderUI({
      createVariableTable(reactive_vars[['selected_vars']])
    })
    
  })
  
  
  
  ### Models > Regression
  regression_build_regression = paste0(regression_prefix, "buttonBuildRegression")
  observeEvent(input[[regression_build_regression]], {
    fit = doRegression(reactive_vars[['selected_vars']])
    
    output_regression_graph_1 = paste0(regression_prefix, "graphResidualVsFitted")
    output[[output_regression_graph_1]] = renderPlot({
      plot(fit[["regression"]], which=1)
    })
    
    output_regression_graph_2 = paste0(regression_prefix, "graphQQ")
    output[[output_regression_graph_2]] = renderPlot({
      plot(fit[["regression"]], which=2)
    })
    
    output_regression_graph_3 = paste0(regression_prefix, "graphStdResVsFitted")
    output[[output_regression_graph_3]] = renderPlot({
      plot(fit[["regression"]], which=3)
    })
    
    output_regression_graph_4 = paste0(regression_prefix, "graphStdResVsLeverage")
    output[[output_regression_graph_4]] = renderPlot({
      plot(fit[["regression"]], which=4)
    })

    fillVariableTable(session, fit[['regression']])
    
  })
  
}

