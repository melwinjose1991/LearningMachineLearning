library(FredR)



## Reading configuration file
config_fetch_data = read.csv("../../Config/fetch_data.csv", header=TRUE, sep=",")
config_fetch_data = config_fetch_data[config_fetch_data$fetch=="yes",]



## Parameters
api.key = "b96673bc1d92a335c6306c864e046827"
date_start = "2013-01-01"
date_end = "2016-12-31"
no_of_data_obs = 48

sa_OR_nsa = "Not Seasonally Adjusted" 
#sa_OR_nsa = "Seasonally Adjusted Annual Rate"

#   source : https://fred.stlouisfed.org/categories
fred = FredR(api.key)



for(sub_category_id in config_fetch_data$sub_category_id){
  
  category_name = config_fetch_data[config_fetch_data$sub_category_id==sub_category_id,"category_name"]
  sub_category_name = config_fetch_data[config_fetch_data$sub_category_id==sub_category_id,"sub_category_name"]
  
  
  print(paste0("Fetching series in ",sub_category_name))
  sub_category = fred$category.series(category_id=sub_category_id)
  #sub_category
  
  
  print(paste0("Filtering series in ",sub_category_name))
  series_id_title = sub_category[ sub_category$frequency=="Monthly" 
                             & sub_category$seasonal_adjustment==sa_OR_nsa, 
                             c("id","title") ]
  #series_id_title
  
  
  for(id in series_id_title$id){
    
    print(paste0("Fetching series : ",id))
    series = fred$series.observations(series_id=id, 
                                      observation_start = date_start,
                                      observation_end = date_end)
    
    is_okay = isSeriesOKAY(series)
    if(is_okay == "yes"){
      if(exists("df_series")){
        df_series[,id]=series$value
      }else{
        df_series = data.frame(date=series$date)
      }
    }else{
      print(paste0(id,is_okay))
    }
    
  }
  
  
  ## Writing Series into CSV
  file = paste0("../../Data/" ,as.character(category_name), "/", as.character(sub_category_name) )
  if(sa_OR_nsa=="Not Seasonally Adjusted"){
    file = paste0(file, "_nsa.csv")
  }else{
    file = paste0(file, "_sa.csv")
  }
  print(paste0("Saving series in file ",file))
  write.csv(df_series, file, quote=FALSE, row.names=FALSE)

  rm(df_series)

}


isSeriesOKAY=function(series){
  
  if(dim(series)[1]!=no_of_data_obs){
    return(paste0(" has ",dim(series)[1]," observations, required ",no_of_data_obs))
  }
  
  if(sum(series$value==".")>0){
    return(paste0(" has some observations == '.' "))
  }
  
  return("yes")
}
