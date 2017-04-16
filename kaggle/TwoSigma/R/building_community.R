library(jsonlite)
library(magrittr)
library(dplyr)
library(purrr)
library(data.table)
library(stringr)
library(tm)
library(ggmap)
library(mclust)
library(fpc)

data = fromJSON("../data/train.json")

data$features <- NULL
data$photos <- NULL 

vars = setdiff(names(data), c("photos", "features"))
data = map_at(data, vars, unlist) %>% tibble::as_tibble(.)



# longitude & latitude
zeros_addrs = data[data$latitude==0 & data$longitude==0,]$street_address
zeros_ny = paste(zeros_addrs,", new york")
zeros_addrs = data.frame("street_address"=zeros_addrs)
coords <- sapply(zeros_ny, function(x) geocode(x, source = "google")) %>%
  t %>%
  data.frame %>%
  cbind(zeros_addrs, .)
rownames(coords) = 1:nrow(coords)

data[data$latitude==0 & data$longitude==0, c("listing_id","latitude","longitude") ]
zeros_id = data$latitude==0 & data$longitude==0
data[zeros_id,"longitude"] = unlist(coords$lon)
data[zeros_id,"latitude"] = unlist(coords$lat)
data[zeros_id,c("listing_id", "street_address", "longitude", "latitude")]


aggBuildings = function(lvl){
  #lvl = "medium"
  df = aggregate(longitude ~ building_id, data[data$interest_level==lvl,], mean)
  df_lat = aggregate(latitude ~ building_id, data[data$interest_level==lvl,], mean)
  
  df["latitude"] = df_lat["latitude"]
  df["interest_level"] = lvl
  #dim(df)
  df
}
means = aggBuildings("high")
means = rbind(means, aggBuildings("medium"))
means = rbind(means, aggBuildings("low"))

calculateDistance=function(x_1, y_1, x_2, y_2){
  sqrt((x_1-x_2)^2+(y_1-y_2)^2)
}

findClosest=function(row_i){
  #row_i = x[1,]
  #print(row_i)
  lon = row_i[2]
  lat = row_i[1]
  dist = apply(means[means$interest_level!="low",c("longitude","latitude")],1,FUN=function(x) calculateDistance(lon,lat,x[1],x[2]))
  #print(dist)
  means[which.min(dist),"interest_level"]
}

apply(data[1:100,c("longitude","latitude")], 1, FUN=findClosest)



lvl = "medium"
lats = means[means$interest_level==lvl,"latitude"]
lons = means[means$interest_level==lvl,"longitude"]
#lats = unlist(lats)
#lons = unlist(lons)
df = data.frame(lon=lons, lat=lats)
plot(df)
