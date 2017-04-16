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



###   Clustering  ###
df=data.frame(lon=data$longitude, lat=data$latitude, interest_level=data$interest_level)
plot(df[,c("lon","lat")])

#quantile(df$lat, probs=seq(0,1,by=0.0025))
#quantile(df$lon, probs=seq(0,1,by=0.0025))

sum(df$lat>=40.58 & df$lat<=40.88)/dim(df)[1]
sum(df$lon>=-74.03 & df$lon<=-73.80)/dim(df)[1]

df=df[df$lat>=40.58 & df$lat<=40.88 & df$lon>=-74.03 & df$lon<=-73.80,]
plot(df[,c("lon","lat")])


getMeans = function(lvl, e=0.001, min_pts=5, just_plot = FALSE){
  lats = df[df$interest_level==lvl,"lat"]
  lons = df[df$interest_level==lvl,"lon"]
  lats = unlist(lats)
  lons = unlist(lons)
  df = data.frame(lon=lons, lat=lats)
  #plot(df)
  model = dbscan(df, eps=e, MinPts = min_pts)
  print(length(unique(model$cluster)))
  plot(model, df)
  
  if(just_plot==FALSE){
    total = length(unique(model$cluster))
    means_lon = c()
    means_lat = c()
    for(i in 1:(total-1)){
      mean = colMeans(df[model$cluster==i,])
      lon = mean[1]
      lat = mean[2]
      means_lon = c(means_lon, lon)
      means_lat = c(means_lat, lat)
    }
    data.frame(lon=means_lon, lat=means_lat, level=lvl)
  }
}
means = getMeans("high", e=0.0001, min_pts=5)

m = getMeans("medium", e=0.0001, min_pts = 5, just_plot = TRUE)
means = rbind(means, m) 

m = getMeans("low", e=0.0001, min_pts = 5, just_plot = TRUE)
means = rbind(means, m) 

calculateDistance=function(x_1, y_1, x_2, y_2){
  sqrt((x_1-x_2)^2+(y_1-y_2)^2)
}

findClosest=function(row_i){
  #row_i = x[1,]
  #print(row_i)
  lon = row_i[2]
  lat = row_i[1]
  dist = apply(means[,c("lat","lon")],1,FUN=function(x) calculateDistance(lon,lat,x[1],x[2]))
  #print(dist)
  means[which.min(dist),"level"]
}

apply(x[,c("latitude","longitude")], 1, FUN=findClosest)
