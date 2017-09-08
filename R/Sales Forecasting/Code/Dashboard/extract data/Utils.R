
### CreateXSubcategories
#     Modfies the meta_data to create more series-id
#     from existing series. Newly created ids are of the 
#     form <id>_x<n> , where n indicates how much the
#     series is shifted from the past. Ex: 1234_x6,
#     is the id for series-1234 that has been shifted
#     by 6 months from the past to the current time i.e
#     the values in 1234_x6 for a month/year stores the
#     value of 1234 6months back. This function only 
#     creates new series-id and their corresponding 
#     titles, it doesn't create the values for them.
#
createXSeriesIDs = function(file, past_months=12){
  
  file = "../../Data/External Data/FRED/meta_data.csv"
  past_months = 12
  
  meta_data = read.csv(file)
  dim(meta_data)
  meta_data_tmp = meta_data
  
  for(month in 1:past_months){

    tmp = meta_data_tmp
    tmp$series_id = paste0(tmp$series_id, "_x", month)
    tmp$title = paste0(tmp$title, " - x", month)
    
    meta_data = rbind(meta_data, tmp)
  }
  
  dim(meta_data)
  
  write.csv(meta_data, file, quote=FALSE, row.names=FALSE)
  
}


### createXSeriesData
#     Creates a new series by shifting the old series.
#     Creates 12 new series for each of the existing series.
createXSeriesData = function(sub_categories_file, past_months=12){
  
  FRED_folder = "../../Data/External Data/FRED/"
  meta_data_file = paste0(FRED_folder, "meta_data.csv")
  sub_categories_file = paste0(FRED_folder, "sub_categories.csv")
  past_months = 12
  
  meta_data_df = read.csv(meta_data_file)
  sub_categories_df = read.csv(sub_categories_file)
  
  for(sub_category_id in unique(meta_data_df$sub_category_id)){
    
    category_name = as.character(sub_categories_df[sub_categories_df$sub_category_id==sub_category_id,
                                   "category_name"])
    sub_category_name = as.character(sub_categories_df[sub_categories_df$sub_category_id==sub_category_id,
                                       "sub_category_name"])
    
    sub_category_file = paste0(FRED_folder, category_name, "/", sub_category_name,"_nsa.csv")
    print(paste0("Reading file : ", sub_category_file))
    sub_category_df = read.csv(sub_category_file)
    
    initial_col_names = colnames(sub_category_df)
    initial_col_names = initial_col_names[-1]
    
    no_of_entries = dim(sub_category_df)[1]
    
    for(series_id in initial_col_names){
      old_series_values = sub_category_df[,series_id]
      for(month in 1:past_months){
        new_series_id = paste0(series_id,"_x",month)
        new_series_values = c(rep(old_series_values[1], month),old_series_values)
        new_series_values = head(new_series_values, no_of_entries)
        
        sub_category_df[, new_series_id] = new_series_values
      }
    }
    dim(sub_category_df)[2]-1 == length(initial_col_names)*13
    
    write.csv(sub_category_df, sub_category_file, row.names=TRUE, quote=TRUE)
    
  }
  
}
