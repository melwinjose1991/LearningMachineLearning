print("Feature Selection :: All Features :: Init")


## Globals
all_features_prefix = paste0(features_prefix, "features_")

#file = paste0("../../Data/External Data/FRED/meta_data.csv")
#meta_data = read.csv(file, header = TRUE, sep = ",")



## UI Elements
initAllFeaturesUI = function(){
  
  months = 1:12
  months = paste0("month",months)
  
  # row:0 - internal features
  id = paste0(all_features_prefix, "month")
  month_internal_features = checkboxGroupInput(id, label="Months", 
                                               choices=months, selected=months, inline=TRUE) 
  
  id = paste0(all_features_prefix, "trend")
  t_internal_feature = checkboxInput(id, label="Trend", value=TRUE)
  
  row_0 = fluidRow(month_internal_features, t_internal_feature)
  
  
  # row:1 - external features
  all_features_id = paste0(all_features_prefix, "all_features_box")
  ui_all_features = uiOutput(all_features_id)
  row_1 = fluidRow(ui_all_features)
  
  tabPanel(title = "Features", row_0, row_1)
}



## Server Function
getCheckBoxInput = function(i, group_id) {
  series_id = meta_data[i,]$series_id
  category_name = meta_data[i,]$category_name
  sub_category_name = meta_data[i,]$sub_category_name
  var_name = meta_data[i,]$title
  
  id = paste0(all_features_prefix, "fId|", group_id, "|", series_id)
  
  checkboxInput(inputId = id,
                label = var_name,
                value = TRUE)
  
}



getGroupCheckBoxInput = function(group) {
  print("Feature Selection :: All Features :: getGroupCheckBoxInput()")

  group_id = unique(meta_data[meta_data$sub_category_name==group, "sub_category_id"])
  type_rows = which(meta_data$sub_category_name == group)
  
  select_all_id = paste0(all_features_prefix, "selectAllId|", group_id)
  select_all = checkboxInput(inputId = select_all_id,
                             label = "Select All",
                             value = TRUE)
  
  show_button_id = paste0(all_features_prefix, "showButtonId|", group_id)
  show_button = actionButton(show_button_id, label="Hide")
  
  vars_columns = lapply(
    type_rows,
    FUN = function(row_index)
      column(width = 2, getCheckBoxInput(row_index, group_id))
  )
  
  group_div_id = paste0(all_features_prefix, "divId_", group_id)
  list(title = tags$h4(group),
       select_all,
       show_button,
       tags$div(id = group_div_id, vars_columns))
  
}



populateFeatures = function(input, output, session){
  
  if(nrow(meta_data)==0){
    meta_data <<- read.csv(paste0(FRED_folder, "/meta_data_old.csv" ))
  }
  
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
    
    group_id = unique(meta_data[meta_data$sub_category_name==group, "sub_category_id"])
    select_all_id = paste0(all_features_prefix, "selectAllId|", group_id)
    show_button_id = paste0(all_features_prefix, "showButtonId|", group_id)
    
    observeEvent(input[[select_all_id]], {
      all_features = vector('character')
      for (key in names(input)) {
        if (grepl(paste0("_fId\\|", group_id,"\\|"), key)) {
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
        showElement(paste0(all_features_prefix, "divId_", group_id))
        updateActionButton(session, show_button_id, label = "Hide")
      } else{
        hideElement(paste0(all_features_prefix, "divId_", group_id))
        updateActionButton(session, show_button_id, label = "Show")
      }
    })
    
  })
  
}

removeUnselectedMonthsT = function(input, x){
  
  months = 2:12
  months = paste0("month",months)
  months_to_include = input[[paste0(all_features_prefix, "month")]]
  months_to_exclude = setdiff(months, months_to_include)
  
  t = input[[paste0(all_features_prefix, "trend")]]
  print(paste0(">>> t:", t, "<<<"))
  
  if(t==FALSE){
    x[ , ! colnames(x) %in% c(months_to_exclude, "t") ]    
  }else{
    x[ , ! colnames(x) %in% c(months_to_exclude) ]
  }
  
}
