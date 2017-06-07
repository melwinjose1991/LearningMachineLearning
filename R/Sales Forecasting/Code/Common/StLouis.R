library(FredR)



## Parameters
api.key = "b96673bc1d92a335c6306c864e046827"
date_start = "2013-01-01"
date_end = "2016-12-31"

main_category_id = 1
sub_category_id = 32436
#sa_OR_nsa = "Not Seasonally Adjusted" 
sa_OR_nsa = "Seasonally Adjusted Annual Rate"



## Fetching Data
#   source : https://fred.stlouisfed.org/categories

fred = FredR(api.key)

sub_category = fred$category.series(category_id=sub_category_id)
sub_category

series_id_title = sub_category[ sub_category$frequency=="Monthly" 
                           & sub_category$seasonal_adjustment==sa_OR_nsa, 
                           c("id","title") ]
series_id_title

for(id in series_id_title$id){
  
  print(paste0("Fetching ",id))
  series = fred$series.observations(series_id=id, 
                                    observation_start = date_start,
                                    observation_end = date_end)
  if(exists("df_series")){
    df_series[,id]=series$value
  }else{
    df_series = data.frame(date=series$date)
  }
  
}


## Writing into CSV
if(sa_OR_nsa=="Not Seasonally Adjusted"){
  write.csv(df_series, "../../Data/Production and Business Activity/Construction_nsa.csv", 
          quote=FALSE, row.names=FALSE)
}else{
  write.csv(df_series, "../../Data/Production and Business Activity/Construction_sa.csv", 
            quote=FALSE, row.names=FALSE)
}