library(jsonlite)
library(magrittr)
library(dplyr)
library(purrr)
library(data.table)
library(stringr)
library(tm)
library(ggmap)
library(mclust)

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


## Clustering
df=data.frame(lon=data$longitude, lat=data$latitude, interest_level=data$interest_level)
plot(df[,c("lon","lat")])

quantile(df$lat, probs=seq(0,1,by=0.0025))
quantile(df$lon, probs=seq(0,1,by=0.0025))

sum(df$lat>=40.58 & df$lat<=40.88)/dim(df)[1]
sum(df$lon>=-74.03 & df$lon<=-73.80)/dim(df)[1]

df=df[df$lat>=40.58 & df$lat<=40.88 & df$lon>=-74.03 & df$lon<=-73.80,]
plot(df[,c("lon","lat")])

# high
lat_high = df[df$interest_level=="high","lat"]
lon_high = df[df$interest_level=="high","lon"]
lat_high = unlist(lat_high)
lon_high = unlist(lon_high)
df_high=data.frame(lon=lon_high, lat=lat_high)
plot(df_high)
model_high = Mclust(df_high, G=17, modelNames = "VVV")
plot(model_high)
mean_all = data.frame(lon=model_high$parameters$mean[1,],lat=model_high$parameters$mean[2,],level="high")

calculateDistance=function(x_1, y_1, x_2, y_2){
  sqrt((x_1-x_2)^2+(y_1-y_2)^2)
}

findClosest=function(row_i){
  row_i = x[1,]
  lon = row_i$longitude
  lat = row_i$latitude
  dist = apply(mean_all[,c("lat","lon")],1,FUN=function(x) calculateDistance(lon,lat,x[1],x[2]))
  mean_all[which.min(dist),"level"]
}


# medium
lat_medium = df[df$interest_level=="medium","lat"]
lon_medium = df[df$interest_level=="medium","lon"]
lat_medium = unlist(lat_medium)
lon_medium = unlist(lon_medium)
df_medium=data.frame(lon=lon_medium, lat=lat_medium)
plot(df_medium)
model_medium = Mclust(df_medium, G=38, modelNames = "VVV")
plot(model_medium)
mean_med = data.frame(lon=model_medium$parameters$mean[1,],lat=model_medium$parameters$mean[2,],level="medium")
mean_all = rbind(mean_all, mean_med)
  
# low
lat_low = df[df$interest_level=="low","lat"]
lon_low = df[df$interest_level=="low","lon"]
lat_low = unlist(lat_low)
lon_low = unlist(lon_low)
df_low=data.frame(lat=lat_low, lon=lon_low)
plot(df_low)
model_low = Mclust(df_low, G=38:100, modelNames = "VVV")
plot(model_low)


