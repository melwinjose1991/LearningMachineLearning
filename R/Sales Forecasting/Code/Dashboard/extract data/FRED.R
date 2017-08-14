library(FredR)



FRED_prefix = paste0(extract_tab_prefix, "FRED_")
FRED_folder = paste0(external_data_folder,"/FRED")

categories_file = paste0(FRED_folder, "/categories.csv" )
sub_categories_file = paste0(FRED_folder, "/sub_categories.csv" )
meta_file = paste0(FRED_folder, "/meta_data.csv" )

meta_data = data.frame()
# stores meta-data of series like id, name, etc

api.key = "b96673bc1d92a335c6306c864e046827"

sa_OR_nsa = "Not Seasonally Adjusted" 
#sa_OR_nsa = "Seasonally Adjusted Annual Rate"

#   source : https://fred.stlouisfed.org/categories
fred = FredR(api.key)



### Fetching available subcategories
#     Reads categories.csv, fetches categories 
#     with fetch=yes. Writes available sub-categories
#     into sub_categories.csv
#   NOTE :  run it once to create sub_categories.csv
#           which is required for the functioning of
#           the tool
if(FALSE){
  
  categories_df = read.csv(categories_file)
  categories_df = categories_df[categories_df$fetch=="yes",]
  
  sub_categories_df = data.frame(category_id=integer(),
                         category_name=character(),
                         sub_category_id=integer(),
                         sub_category_name=character())
  
  for(category_id in categories_df$category_id){
    category_name = categories_df[categories_df$category_id==category_id,"category_name"]
    sub_categories = fred$category.children(category_id)
    rows= nrow(sub_categories)
    
    temp_df = data.frame(category_id=rep(category_id, rows),
                         category_name=rep(category_name, rows),
                         sub_category_id=sub_categories$id,
                         sub_category_name=sub_categories$name)
    
    sub_categories_df = rbind(sub_categories_df, temp_df)
  }

  write.csv(sub_categories_df, sub_categories_file, quote=FALSE, row.names=FALSE)
  
}



## UI Elements
getFREDUI = function(){
  
  # Row - 1 :: Show available categories
  sub_categories_df = read.csv(sub_categories_file)
  sub_categories_rows = apply(sub_categories_df, 1, function(row){
    
    var_id = paste0(FRED_prefix,"subCatId|",as.numeric(row['sub_category_id']))
    
    text_var_name = tags$h5(row['sub_category_name'])
    col_var_name = column(width=6, text_var_name)
    
    var_select_id = paste0(var_id,"|Select")
    checkbox_var_select = checkboxInput(var_select_id, "", value=FALSE)
    col_var_select = column(width=2, checkbox_var_select)
    
    list(col_var_name, col_var_select)
  })
  row_1 = fluidRow(sub_categories_rows)
  
  # Row -2 :: Fetch Button
  fetch_button_id = paste0(FRED_prefix,"fetchData")
  fetch_button = actionButton(fetch_button_id, "Fetch")
  row_2 = fluidRow(fetch_button)
  
  tabPanel(title="FRED", row_1, row_2)
}



## Server Functions

# Checking if the series has enough data points
# and missing values
isSeriesOKAY=function(series){
  
  if(dim(series)[1]!=product_data_obeservations){
    return(paste0(" has ",dim(series)[1],
                  " observations, required ",product_data_obeservations))
  }
  
  if(sum(series$value==".")>0){
    return(paste0(" has some observations == '.' "))
  }
  
  return("yes")
}



attachFREDObservers = function(input){
  
  fetch_button_id = paste0(FRED_prefix,"fetchData")
  observeEvent(input[[fetch_button_id]], {
    
    input_value = reactiveValuesToList(input)
    sub_categories_id = vector('character')
    for (key in names(input_value)) {
      if( grepl("subCatId",key) && input_value[[key]] == TRUE) {
        sub_categories_id = c(sub_categories_id, unlist(strsplit(key, "\\|"))[2])
      }
    }
    print(sub_categories_id)
    
    fetchData(sub_categories_id)
    
  })
}



### Fetches the data from FRED
#     Fetches the sub-categories selected by user, 'sub_categories_id',
#     and writes the series into respective files
fetchData = function(sub_categories_id){
  
  # Contains ids & names of sub-categories and categories
  config_fetch_data = read.csv(sub_categories_file)
  
  # TOD: no of days in a month 29,30,31
  # Format : yyyy-mm-dd
  start_month = ifelse(product_start_date[2]<10, 
                       paste0("0",product_start_date[2]), 
                       product_start_date[2])
  end_month = ifelse(product_end_date[2]<10, 
                       paste0("0",product_end_date[2]), 
                       product_end_date[2])
  
  date_start = paste0(product_start_date[1], "-", start_month, "-01")
  date_end = paste0(product_end_date[1], "-", end_month, "-31")
  
  meta_data = data.frame(category_id=integer(),
                         category_name=character(),
                         sub_category_id=integer(),
                         sub_category_name=character(),
                         series_id=integer(),
                         title=character())
  
  for(sub_category_id in sub_categories_id){
    
    category_id = config_fetch_data[config_fetch_data$sub_category_id==sub_category_id,
                                    "category_id"]
    category_name = config_fetch_data[config_fetch_data$sub_category_id==sub_category_id,
                                      "category_name"]
    sub_category_name = config_fetch_data[config_fetch_data$sub_category_id==sub_category_id,
                                          "sub_category_name"]
    
    
    print(paste0("Fetching series in ",sub_category_name))
    sub_category = fred$category.series(category_id=sub_category_id)
    #sub_category
    
    
    print(paste0("Filtering series in ",sub_category_name))
    series_id_title = sub_category[ sub_category$frequency=="Monthly" 
                                    & sub_category$seasonal_adjustment==sa_OR_nsa, 
                                    c("id","title") ]
    #series_id_title
    
    print(paste0("Fetching #",dim(series_id_title)[1]," series"))  
    for(series_id in series_id_title$id){
      
      series_name = series_id_title[series_id_title$id==series_id, "title"]
      series_name = gsub(",", "-", series_name)
      
      print(paste0("Fetching series : ", series_id))
      series = fred$series.observations(series_id=series_id, 
                                        observation_start = date_start,
                                        observation_end = date_end)
      is_okay = isSeriesOKAY(series)
      if(is_okay == "yes"){
        
        if(exists("df_series")){
          # df_series data.frame stores the series
          # from a sub-category
          df_series[,series_id]=series$value
        }else{
          df_series = data.frame(date=series$date)
          df_series[,series_id]=series$value
        }
        
        meta = data.frame(category_id=category_id, category_name=category_name,
                          sub_category_id=sub_category_id, sub_category_name,
                          series_id=series_id, title=series_name)
        
        meta_data = rbind(meta_data, meta)
        
      }else{
        print(paste0(series_id,is_okay))
      }
      
    }
    
    
    ## Writing Series into CSV
    file = paste0(FRED_folder, "/", as.character(category_name), 
                  "/", as.character(sub_category_name) )
    if(sa_OR_nsa=="Not Seasonally Adjusted"){
      file = paste0(file, "_nsa.csv")
    }else{
      file = paste0(file, "_sa.csv")
    }
    print(paste0("Saving series in file ",file))
    write.csv(df_series, file, quote=FALSE, row.names=FALSE)
    
    rm(df_series)
    
  }
  
  write.csv(meta_data, meta_file, quote=FALSE, row.names=FALSE)
  
  meta_data <<- meta_data
  
}