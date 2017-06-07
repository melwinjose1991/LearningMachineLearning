library(FredR)



api.key = "b96673bc1d92a335c6306c864e046827"
date_start = "2013-01-01"
date_end = "2016-12-31"

fred = FredR(api.key)



## Sample Code
main_category = fred$category.children(category_id=1)
# 1 : Production & Business Activity

sub_category = fred$category.series(category_id=32436)
# 32436 : Construction

# Series that are have monthly reading and
# that are not Not Seasonally Adjusted
series_nsa = sub_category[sub_category$frequency=="Monthly" 
             & sub_category$seasonal_adjustment=="Not Seasonally Adjusted", c("id","title") ]


# Fetching the values for 
series_nsa[1,"id"]
series = fred$series.observations(series_id=series_nsa[1,"id"], 
                                      observation_start = date_start,
                                      observation_end = date_end)
series



## Construction
sub_category = fred$category.series(category_id=32436)

series_nsa = sub_category[ sub_category$frequency=="Monthly" 
                           & sub_category$seasonal_adjustment=="Not Seasonally Adjusted", 
                           c("id","title") ]

df = data.frame(date=series$date)
for(id in series_id_nsa$id){
  print(paste0("Fetching ",id))
  series = fred$series.observations(series_id=id, 
                                    observation_start = date_start,
                                    observation_end = date_end)
  df[,id]=series$value
}
write.csv(df, "../Production and Business Activity/Construction.csv", 
          quote=FALSE, row.names=FALSE)
