print("Feature Selection :: All Features :: Init")


## Globals
features_prefix = paste0(feature_selection_prefix, "features_")

file = paste0("../../Data/", product, "/meta_data.csv")
meta_data = read.csv(file, header = TRUE, sep = ",")



## UI Elements
features_buttonShow = actionButton(inputId = paste0(features_prefix, "buttonShow"),
                                   label = "Show")

features_buttonSelect = actionButton(inputId = paste0(features_prefix, "buttonSelect"),
                                     label = "Select Checked")

featureSelection_tabAllFeatures = tabPanel(title = "Features",
                                           features_buttonShow,
                                           features_buttonSelect,
                                           uiOutput("all_features_box"))



## Helper Function
getCheckBoxInput = function(i) {
  series_id = meta_data[i, ]$series_id
  category_name = meta_data[i, ]$category_name
  sub_category_name = meta_data[i, ]$sub_category_name
  var_name = meta_data[i, ]$title
  
  id = paste0(features_prefix, "fId|", series_id)
  
  checkboxInput(inputId = id,
                label = var_name,
                value = TRUE)
  
}



getGroupCheckBoxInput = function(group) {
  print("Feature Selection :: All Features :: getGroupCheckBoxInput()")
  
  type_rows = which(meta_data$sub_category_name == group)
  
  id = paste0(features_prefix, group, "SelectAll")
  select_all = checkboxInput(inputId = id,
                             label = "Select All",
                             value = TRUE)
  
  vars_columns = lapply(
    type_rows,
    FUN = function(row_index)
      column(width = 2, getCheckBoxInput(row_index))
  )
  
  list(title = tags$h4(group),
       select_all,
       tags$div(id = group, vars_columns))
  
}
