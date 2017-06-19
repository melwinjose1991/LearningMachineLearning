print("Feature Selection :: All Features :: Init")


## Globals
features_prefix = paste0(feature_selection_prefix, "features_")

file = paste0("../../Data/", product, "/meta_data.csv")
meta_data = read.csv(file, header = TRUE, sep = ",")



## UI Elements
featureSelection_tabAllFeatures = tabPanel(title = "Features",
                                           uiOutput("all_features_box"))



## Helper Function
getCheckBoxInput = function(i, group) {
  series_id = meta_data[i,]$series_id
  category_name = meta_data[i,]$category_name
  sub_category_name = meta_data[i,]$sub_category_name
  var_name = meta_data[i,]$title
  
  id = paste0(features_prefix, "fId|", group, "|", series_id)
  
  checkboxInput(inputId = id,
                label = var_name,
                value = TRUE)
  
}



getGroupCheckBoxInput = function(group) {
  print("Feature Selection :: All Features :: getGroupCheckBoxInput()")
  
  type_rows = which(meta_data$sub_category_name == group)
  
  select_all_id = paste0(features_prefix, "selectAllId|", group)
  select_all = checkboxInput(inputId = select_all_id,
                             label = "Select All",
                             value = TRUE)
  
  show_button_id = paste0(features_prefix, "showButtonId|", group)
  show_button = actionButton(show_button_id, label="Hide")
  
  vars_columns = lapply(
    type_rows,
    FUN = function(row_index)
      column(width = 2, getCheckBoxInput(row_index, group))
  )
  
  group_div_id = paste0(features_prefix, "divId_", group)
  list(title = tags$h4(group),
       select_all,
       show_button,
       tags$div(id = group_div_id, vars_columns))
  
}
