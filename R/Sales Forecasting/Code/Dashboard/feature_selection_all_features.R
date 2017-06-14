print("Feature Selection :: All Features :: Init")


file = paste0("../../Data/", product, "/meta_data.csv")
meta_data = read.csv(file, header=TRUE, sep=",")



getCheckBoxInput = function(i){
  
  series_id = meta_data[i,]$series_id
  category_name = meta_data[i,]$category_name
  sub_category_name = meta_data[i,]$sub_category_name
  var_name = meta_data[i,]$title

  id = paste0("all_features_", series_id)
  
  checkboxInput(inputId=id, label=var_name, value=TRUE)
  
}



getGroupCheckBoxInput = function(group){
  print("Feature Selection :: All Features :: getGroupCheckBoxInput()")
  
  type_rows = which(meta_data$sub_category_name==group)  
  
  list(title = tags$h4(group),
               lapply(type_rows, FUN=function(row_index) column(width=2, getCheckBoxInput(row_index)) )
       )
  
}




