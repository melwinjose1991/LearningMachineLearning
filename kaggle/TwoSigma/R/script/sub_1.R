library(jsonlite)
library(magrittr)
library(dplyr)
library(purrr)
library(data.table)
library(stringr)
library(tm)
library(ggmap)
library(xgboost)
library(caret)

data = fromJSON("../../data/train.json")
real_test = fromJSON("../../data/test.json")


data$features <- NULL
data$photos <- NULL 
real_test$features <- NULL
real_test$photos <- NULL 


vars = setdiff(names(data), c("photos", "features"))
data = map_at(data, vars, unlist) %>% tibble::as_tibble(.)
real_test = map_at(real_test, vars, unlist) %>% tibble::as_tibble(.)


if(TRUE){
  
  ## Similar to Enrique Perez HerreroRental's Listing NY Map
  ## Have added more places.
  
  ## longitude & latitude
  
  # getting lat & long of places for which they are zero
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
  rm(zeros_id)
  
  
  # removing outliers
  #quantile(data$latitude, probs=seq(0,1,by=0.00125))
  #sum(data$latitude<=39.58 | data$latitude>=41.88)
  data = data[!(data$latitude<=39.58 | data$latitude>=41.88), ]
  
  #quantile(data$longitude , probs=seq(0,1,by=0.00125))
  #sum(data$longitude<=-74.23 | data$longitude>=-73.60)
  data = data[!(data$longitude<=-74.23 | data$longitude>=-73.60), ]
  
  
  getDistance = function(df, loc_lat, loc_lon){
    mapply(function(lon, lat) sqrt((lon - loc_lon)^2  + (lat - loc_lat)^2),
           df$longitude, df$latitude) 
  }
  
  places = list(
    ## largest 5 parks
    "park_central"=c(40.785091, -73.968285), 
    "park_prospect"=c(40.660204, -73.968956),
    "park_pelham"=c(40.850569, -73.821018), 
    "park_greenbelt"=c(40.591831, -74.139199),
    "park_van_cortlandt"=c(40.897935, -73.885951), 
    "park_flushing"=c(40.739714, -73.840785),
    
    ## universities based on student population
    "univ_nyu"=c(40.729513, -73.9964610),         # New York University
    "univ_borough_comm"=c(40.718780, -74.011878), # Borough of Manhattan Community College
    "univ_columbia"=c(40.807536, -73.962573),     # Columbia Unviersity
    "univ_hunter"=c(40.768541, -73.964625),       # Hunter College
    "univ_kingsborough"=c(40.578522, -73.934690), # Kingsborough Community College
    "univ_bernard_m"=c(40.740199, -73.983374),    # Bernard M Baruch College
    "univ_brooklyn"=c(40.630995, -73.954412),     # Brooklyn College, New York
    "univ_ny_cc_tech"=c(40.695534, -73.987459),   # New York City College of Tech 
    "univ_city_college"=c(40.820047, -73.949272), # City College
    "univ_touro"=c(40.742247, -73.990653),        # Touro College, NY
    "univ_john_jay"=c(40.770393, -73.988499),     # John Jay College of Criminal Justice
    "univ_pace"=c(40.711120, -74.004857),         # Pace university
    "univ_cuny"=c(40.750630, -73.973418), 
    "univ_yesh"=c(40.850485, -73.929107),
    "univ_the_new"=c(40.735501, -73.997138),      # The New School
    "univ_fashion_IT"=c(40.747484, -73.995082),    # Fashion Inst of Tech
    
    ## top rated subway stations
    "subway_times_square" = c(40.755223, -73.987402),
    "subway_grand_central" = c(40.752397, -73.977469),
    "subway_herald_square" = c(40.752397, -73.977469),
    "subway_union_square" = c(40.735284, -73.991058),
    "subway_penn" = c(40.750754, -73.990383)
    
  )
  
  for(place in names(places)){
    coords = places[[place]]
    data[,place] = getDistance(data, coords[1], coords[2])
  }
  
  
  zeros_addrs = real_test[real_test$latitude==0 & real_test$longitude==0,]$street_address
  zeros_ny = paste(zeros_addrs,", new york")
  zeros_addrs = data.frame("street_address"=zeros_addrs)
  coords <- sapply(zeros_ny, function(x) geocode(x, source = "google")) %>%
    t %>%
    data.frame %>%
    cbind(zeros_addrs, .)
  rownames(coords) = 1:nrow(coords)
  
  real_test[real_test$latitude==0 & real_test$longitude==0, c("listing_id","latitude","longitude") ]
  zeros_id = real_test$latitude==0 & real_test$longitude==0
  real_test[zeros_id,"longitude"] = unlist(coords$lon)
  real_test[zeros_id,"latitude"] = unlist(coords$lat)
  real_test[zeros_id,c("listing_id", "street_address", "longitude", "latitude")]
  rm(zeros_id)
  
  for(place in names(places)){
    coords = places[[place]]
    real_test[,place] = getDistance(real_test, coords[1], coords[2])
  }
  
  
  x_cols = c( "bathrooms", "bedrooms", names(places) )


}else{

    x_cols = c( "bathrooms", "bedrooms") 

}


y_col = c("interest_level")

rows = dim(data)[1]

trainAndTestXGB=function(nrounds=800, depth=8, min_rows=200, col_sample=0.5, 
                         row_sample=0.5, learn_rate=0.025, seed=1234, dtrain, dval, dtest){
  set.seed(seed)
  xgb_params = list(
    colsample_bytree= col_sample,
    subsample = row_sample,
    eta = learn_rate,
    objective= 'multi:softprob',
    max_depth= depth,
    min_child_weight= min_rows,
    eval_metric= "mlogloss",
    num_class = 3,
    seed = seed
  )
  
  xgb_model = xgb.train(params = xgb_params,
                        data = dtrain,
                        nrounds = nrounds,
                        watchlist = list(train = dtrain, val=dval),
                        print_every_n = 25,
                        early_stopping_rounds=50)
  
  all_predictions =  (as.data.frame(matrix(predict(xgb_model, dtest), nrow=dim(dtest), byrow=TRUE)))
}

y = as.integer(factor(data$interest_level))
y = y - 1

kfolds = 10
folds = createFolds(y, k = kfolds, list = TRUE, returnTrain = FALSE)

fold = as.numeric(unlist(folds[1]))
x_train = data[-fold, x_cols] 
x_val = data[fold, x_cols]    

y_train = y[-fold]
y_val = y[fold]

# training XGB
dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dval  = xgb.DMatrix(as.matrix(x_val), label=y_val)
dtest = xgb.DMatrix(data.matrix(real_test[,x_cols]))

depth = as.integer(length(x_cols)/2)
nrounds = 50
pred = trainAndTestXGB(nrounds=nrounds, depth=depth, seed=1234, dtrain=dtrain, dval=dval, dtest=dtest)

pred = cbind(pred, real_test$listing_id)
names(pred) = c("high","low","medium","listing_id")
pred = pred[,c(1,3,2,4)]
head(pred)

file_name = paste0("xgb_", "x",as.String(length(x_cols)), "_d",depth, "_r",nrounds, ".csv")
write.csv(pred, file_name, row.names = FALSE)

## Execute on your machine with the if(){...} made to TRUE and nrounds=800
## public leaderboard score : 0.73201