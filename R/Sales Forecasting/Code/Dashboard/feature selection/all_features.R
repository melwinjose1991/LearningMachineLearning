print("Feature Selection :: All Features :: Init")


## Globals
all_features_prefix = paste0(features_prefix, "features_")

file = paste0("../../Data/", product, "/meta_data.csv")
meta_data = read.csv(file, header = TRUE, sep = ",")



## UI Elements
initAllFeaturesUI = function(){
  
  all_features_id = paste0(all_features_prefix,"all_features_box")
  tab_all_features = tabPanel(title = "Features",
                              uiOutput(all_features_id))
  tab_all_features
}



## Server Function
getCheckBoxInput = function(i, group) {
  series_id = meta_data[i,]$series_id
  category_name = meta_data[i,]$category_name
  sub_category_name = meta_data[i,]$sub_category_name
  var_name = meta_data[i,]$title
  
  id = paste0(all_features_prefix, "fId|", group, "|", series_id)
  
  checkboxInput(inputId = id,
                label = var_name,
                value = TRUE)
  
}



getGroupCheckBoxInput = function(group) {
  print("Feature Selection :: All Features :: getGroupCheckBoxInput()")
  
  type_rows = which(meta_data$sub_category_name == group)
  
  select_all_id = paste0(all_features_prefix, "selectAllId|", group)
  select_all = checkboxInput(inputId = select_all_id,
                             label = "Select All",
                             value = TRUE)
  
  show_button_id = paste0(all_features_prefix, "showButtonId|", group)
  show_button = actionButton(show_button_id, label="Hide")
  
  vars_columns = lapply(
    type_rows,
    FUN = function(row_index)
      column(width = 2, getCheckBoxInput(row_index, group))
  )
  
  group_div_id = paste0(all_features_prefix, "divId_", group)
  list(title = tags$h4(group),
       select_all,
       show_button,
       tags$div(id = group_div_id, vars_columns))
  
}



populateFeatures = function(input, output, session){
  
  ## Populate the features in the "Feature_Selection > Features"
  all_features_id = paste0(all_features_prefix,"all_features_box")
  output[[all_features_id]] = renderUI({
    lapply(
      unique(meta_data$sub_category_name),
      FUN = function(x)
        fluidRow(getGroupCheckBoxInput(x))
    )
  })
  
  
  ## Attaching listeners to each features
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
      if (input[[show_button_id]] %% 2 == 0) {
        showElement(paste0(all_features_prefix, "divId_", group))
        updateActionButton(session, show_button_id, label = "Hide")
      } else{
        hideElement(paste0(all_features_prefix, "divId_", group))
        updateActionButton(session, show_button_id, label = "Show")
      }
    })
    
  })
  
}
