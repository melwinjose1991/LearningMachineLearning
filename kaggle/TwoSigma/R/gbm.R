library(jsonlite)
library(magrittr)
library(dplyr)
library(purrr)
library(h2o)
library(data.table)
library(stringr)
library(tm)
library(ggmap)
library(nnet)
library(xgboost)
library(caret)

data = fromJSON("../data/train.json")
real_test = fromJSON("../data/test.json")


data_features <- data$features
data_photos <- data$photos
data$features <- NULL
data$photos <- NULL 

test_features <- real_test$features
test_photos <- real_test$photos
real_test$features <- NULL
real_test$photos <- NULL 


vars = setdiff(names(data), c("photos", "features"))
data = map_at(data, vars, unlist) %>% tibble::as_tibble(.)
real_test = map_at(real_test, vars, unlist) %>% tibble::as_tibble(.)


data$features <- data_features
data$photos <- data_photos

real_test$features = test_features
real_test$photos = test_photos

rm(data_features)
rm(data_photos)
rm(test_features)
rm(test_photos)



## Helper functions
getAccuracy=function(predictions, actual){
  percent = mean(predictions == actual)
  print(paste('Accuracy',percent))
}

getPivotTableAvg = function(variable, target){
  pivot_1 = aggregate(target~variable, data, FUN=sum)
  pivot_2 = aggregate(target~variable, data, FUN=length)
  pivot_2["target"] = pivot_1["target"]/pivot_2["target"]
  pivot_2
}

getWordCount=function(target, N){
  df = as.data.frame(sort(table(tolower(unlist(data[data$interest_level==target,"features"],' '))), 
                          decreasing = TRUE)[1:N])
  df$total = lapply(df$Var1, FUN=function(x) length(grep(x,tolower(data$features))) )
  df$total = as.numeric(df$total)
  df$per = df$Freq / df$total
  df
}

createFeatureCol=function(){
  for(f in freq_features){
    print(f)
    data[f] = rep(0, rows)
    data[getRows(f),f] = 1
    data[f] = factor(data[[f]])
  }
}

uniqueWords = function(d) {
  return(paste(unique(strsplit(d, " ")[[1]]), collapse = ' '))
}

getNFrequentWords=function(df, var_col, var_str, rows_to_use=0.50, int_lvl, N=0.75){
  
  type_rows = df$interest_level == int_lvl
  df = df[ type_rows, c(var_str,"interest_level")]

  print("Converting to Corpus")
  rows_to_use = dim(df)[1] * rows_to_use
  print(rows_to_use)
  x_cor = Corpus(DataframeSource(df[1:rows_to_use,]))
  print("Applying transformations toLower & removeWords")
  x_cor = tm_map(x_cor, content_transformer(tolower)) 
  x_cor = tm_map(x_cor, removeWords, stopwords("english")) 
  print("Applying transformations removePunctuation")
  x_cor = tm_map(x_cor, removePunctuation) 
  x_cor = tm_map(x_cor, removeNumbers) 
  print("Applying transformations stripWhitespace")
  x_cor = tm_map(x_cor, stripWhitespace) 
  x_cor = tm_map(x_cor, content_transformer(uniqueWords))
  print("Converting to DocumentTermMatrix")
  dtm =  DocumentTermMatrix(x_cor)
  print("Calculating Frequencies")
  print(rows_to_use*N)
  findFreqTerms(dtm, rows_to_use*N)
}
# high : 
# medium : kitchen
# low : 


## Feature Engineering
# initial state of data
init_rows = dim(data)[1]

#bathrooms
data$bathrooms = as.numeric(as.character(data$bathrooms))
data = data[!data$bathrooms>=6, ]  # outliers

real_test$bathrooms = as.numeric(as.character(real_test$bathrooms))


# bedrooms
data$bedrooms = as.numeric(as.character(data$bedrooms))
data = data[!data$bedrooms>=7, ]  # outliers

real_test$bedrooms = as.numeric(as.character(real_test$bedrooms))


# price_diff_bedrooms
rm(mean)
all_ = rbind(data[,c("bedrooms","price")], real_test[,c("bedrooms","price")])
mean_bedroooms_price = as.data.frame(aggregate(price~bedrooms, all_, mean))
rm(all_)

getPriceDiff = function(bedrooms, price){
  mean_price = mean_bedroooms_price[mean_bedroooms_price$bedrooms==bedrooms,"price"]
  price - mean_price
}
data$price_diff_bedrooms = mapply(function(bedrooms, p) getPriceDiff(bedrooms,p), data$bedrooms, data$price)

real_test$price_diff_bedrooms = mapply(function(bedrooms, p) getPriceDiff(bedrooms,p), real_test$bedrooms, real_test$price)


# total rooms
data$rooms = data$bathrooms + data$bedrooms
data = data[!data$rooms>=10, ]

real_test$rooms = real_test$bathrooms + real_test$bedrooms


# price_diff_rooms
all_ = rbind(data[,c("rooms","price")], real_test[,c("rooms","price")])
mean_rooms_price = as.data.frame(aggregate(price~rooms, all_, mean))
rm(all_)

getPriceDiff = function(rooms, price){
  mean_price = mean_rooms_price[mean_rooms_price$rooms==rooms,"price"]
  price - mean_price
}
data$price_diff_rooms = mapply(function(rooms, p) getPriceDiff(rooms,p), data$rooms, data$price)

real_test$price_diff_rooms = mapply(function(rooms, p) getPriceDiff(rooms,p), real_test$rooms, real_test$price)


# bedrooms/bathrooms
data$bathbed = data$bathrooms / data$bedrooms
data[is.na(data$bathbed), c("bathbed")] = -1
data[is.infinite(data$bathbed), c("bathbed")] = 100
#data$bathbed = floor(data$bathbed)

real_test$bathbed = real_test$bathrooms / real_test$bedrooms
real_test[is.na(real_test$bathbed), c("bathbed")] = -1
real_test[is.infinite(real_test$bathbed), c("bathbed")] = 100
#real_test$bathbed = floor(real_test$bathbed)


# price
data$price = as.numeric(as.character(data$price))
data = data[!data$price>30000,]
data = data[!data$price<700,]
data$log_price = log(data$price)
#quantile(data$price, probs=seq(0,1,by=0.0025))

real_test$price = as.numeric(as.character(real_test$price))
real_test$log_price = log(real_test$price)


# price/bedrooms
data$bed_price = data$price / data$bedrooms
data[which(is.infinite(data$bed_price)),"bed_price"] = data[which(is.infinite(data$bed_price)),"price"]

real_test$bed_price = real_test$price / real_test$bedrooms
real_test[which(is.infinite(real_test$bed_price)),"bed_price"] = real_test[which(is.infinite(real_test$bed_price)),"price"]


# price/rooms
data$room_price = data$price / data$rooms
data[which(is.infinite(data$room_price)),"room_price"] = data[which(is.infinite(data$room_price)),"price"]

real_test$room_price = real_test$price / real_test$rooms
real_test[which(is.infinite(real_test$room_price)),"room_price"] = real_test[which(is.infinite(real_test$room_price)),"price"]


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

plot(data[,c("longitude","latitude")])
  # removing outliers
#quantile(data$latitude, probs=seq(0,1,by=0.00125))
#sum(data$latitude<=39.58 | data$latitude>=41.88)
data = data[!(data$latitude<=39.58 | data$latitude>=41.88), ]

#quantile(data$longitude , probs=seq(0,1,by=0.00125))
#sum(data$longitude<=-74.23 | data$longitude>=-73.60)
data = data[!(data$longitude<=-74.23 | data$longitude>=-73.60), ]
plot(data[,c("longitude","latitude")])

plot(real_test[,c("longitude","latitude")])


getDistance = function(df, loc_lat, loc_lon){
  mapply(function(lon, lat) sqrt((lon - loc_lon)^2  + (lat - loc_lat)^2),
         df$longitude, df$latitude) 
}
places = list(
              ## largest 5 parks
              #"park_central"=c(40.785091, -73.968285), 
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
              #"univ_cuny"=c(40.750630, -73.973418), 
              #"univ_yesh"=c(40.850485, -73.929107),
              "univ_the_new"=c(40.735501, -73.997138),      # The New School
              "univ_fashion_IT"=c(40.747484, -73.995082),    # Fashion Inst of Tech
              
              ## top rated subway stations
              "subway_times_square" = c(40.755223, -73.987402),
              "subway_grand_central" = c(40.752397, -73.977469),
              #"subway_herald_square" = c(40.752397, -73.977469),
              "subway_union_square" = c(40.735284, -73.991058),
              "subway_penn" = c(40.750754, -73.990383)
              
              ## most expensive neighborhoods
              #"xpnsv_madison"=c(40.777613, -73.961179),       # Madison Ave
              #"xpnsv_met_college_ny"=c(40.708723,-74.014951), # Metropolitan College of New York 
              #"xpnsv_N_end_avenue"=c(40.715816, -74.015383),  # North End Ave
              
              # safest neighborhoods
              #"safe_sutton"=c(40.769449, -73.951862),       # Sutton Place
              #"safe_battery_park"=c(40.703277, -74.017028), # Battery Park
              #"safe_carnegie_hill"=c(40.784465, -73.955086),# Carnegie Hill
              #"safe_turdor"=c(40.748849,-73.971616), # Turdor City
              #"safe_roosevelt_is"=c(40.760503, -73.950993) # Roosevelt Island

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

# created ???


# feature length
data$f_len = lengths(data$features)
real_test$f_len = lengths(real_test$features)


# num of pictures
data$nphotos = lengths(data$photos)
data$photos = NULL

real_test$nphotos = lengths(real_test$photos)
real_test$photos = NULL

# manager_id
if(FALSE){
  manager_score = list()
  ids = unique(data$manager_id)
  for(id in ids){
    count = sum(data$manager_id==id)
    high_p = sum(data$manager_id==id & data$interest_level=="high") / count
    med_p = sum(data$manager_id==id & data$interest_level=="medium") / count 
    low_p = sum(data$manager_id==id & data$interest_level=="low") / count
    manager_score[[id]]=c(low_p, med_p, high_p, count)
  }
  data$low_score = sapply(data$manager_id, FUN=function(m_id) manager_score[[as.String(m_id)]][1])
  data$med_score = sapply(data$manager_id, FUN=function(m_id) manager_score[[as.String(m_id)]][2])
  data$high_score = sapply(data$manager_id, FUN=function(m_id) manager_score[[as.String(m_id)]][3])
  data$m_count = sapply(data$manager_id, FUN=function(m_id) manager_score[[as.String(m_id)]][4])
  data$mngr_skill = mapply(function(high,med) 2*high+med, data$high_score, data$med_score)
  
  ranked_m = data$m_count>20
  m_mean_high = mean(data[ranked_m & data$interest_level=="high",]$high_score)
  m_mean_med = mean(data[ranked_m & data$interest_level=="medium",]$med_score)
  m_mean_low = mean(data[ranked_m & data$interest_level=="low",]$low_score)
  m_mean_skill = mean(data[ranked_m,]$mngr_skill)
  
  data[!ranked_m,"mngr_skill"] = m_mean_skill
  
  getMScore=function(id,index){
    #print(id)
    id = as.String(id)
    if(id %in% names(manager_score)){
      manager_score[[id]][index]
    }else{
      score=0
      score    
    }
  }
  real_test$low_score = sapply(real_test$manager_id, FUN=function(m_id) getMScore(m_id,1))
  real_test$med_score = sapply(real_test$manager_id, FUN=function(m_id) getMScore(m_id,2))
  real_test$high_score = sapply(real_test$manager_id, FUN=function(m_id) getMScore(m_id,3))
  real_test$m_count = sapply(real_test$manager_id, FUN=function(m_id) getMScore(m_id,4))
  real_test$mngr_skill = mapply(function(high,med) 2*high+med, real_test$high_score, real_test$med_score)
  
  sum(real_test$mngr_skill==0)
  real_test[real_test$mngr_skill==0,"mngr_skill"] = m_mean_skill
  
  rm(manager_score)
  rm(ids)
}

getFactors=function(df1_col, df2_col){
  all_ids = unique(c(df1_col, df2_col))
  all_factors = factor(all_ids)
}


m_factors = getFactors(data$manager_id, real_test$manager_id)
data[,'manager_id_int'] = as.numeric(factor(data$manager_id, levels = m_factors))
real_test[,'manager_id_int'] = as.numeric(factor(real_test$manager_id, levels = m_factors))

data$manager_id = factor(data$manager_id)
real_test$manager_id = factor(real_test$manager_id)


## duplicate features ## 

# x = as.data.frame(sort(table(tolower(unlist(data[,"features"]))), decreasing = TRUE))
# x[1:50,]

# 1556 features length(unique(unlist(data[,"features"])))
# 1294 lowered features length(unique(tolower(unlist(data[,"features"]))))

# y = as.data.frame(sort(table(tolower(unlist(data$features))), decreasing = TRUE))
# y[grepl("doorman",y$Var1) & y$Freq>1,]

freq_features = c(
  ###"elevator",
  "hardwood floors",
  ###"doorman", #"24hr doorman","part time doorman",
  "laundry", "common laundry", ###"private laundry",
  "no fee",
  #x4"pre-war", #"post-war",
  #x4"reduced fee",
  
  ##"loft",
  #"closet","walk in closet",
  #"marble bath",
  ##"ss", #stainless steel kitchen
  #"wifi",
  #"green building",
  #"granite kitchen",
  #"subway",
  #x4"cats allowed",
  #x4"dogs allowed",
  "furnished"
  #"multi-level",
  #"high ceilings",
  #"garage", "private garage",
  #"parking", #"private parking",
  ##"roof deck",
  ###"outdoor space", ##"private outdoor space", ##"common outdoor space", 
  ##"dining room",
  #"high speed internet",
  #"balcony", #"private balcony",
  #"swimming pool",
  ##"new construction", #"newly renovated",
  #"terrace",
  ##"exclusive",
  #"garden",#"common garden","private garden",
  #"wheelchair access",
  #"fireplace",
  #"simplex",
  #"lowrise",#"highrise","midrise",
  #x4"dishwasher"
  ##"fitness center"
  
  #"photos",
  #"playroom",
  
  #"long string"
)

freq_features_map = c(
  "hardwood"="hardwood floors",
  "hardwood floor"="hardwood floors",
  "hardwood flooring"="hardwood floors",
  
  #"full-time doorman"="24hr doorman",
  #"ft doorman" = "24hr doorman",
  #"24/7 doorman"= "24hr doorman",
  #"24/7 doorman concierge"= "24h doorman",
  #"24-hour doorman"= "24hr doorman",
  #"24 hour doorman"= "24hr doorman",
  #"24 hr doorman"= "24hr doorman",
  #"24/7 full-time doorman concierge" = "24hr doorman",
  #"twenty-four hour concierge and doorman" = "24hr doorman",
  
  "full-time doorman"="doorman",
  "ft doorman" = "doorman",
  "24/7 doorman"= "doorman",
  "24/7 doorman concierge"= "doorman",
  "24-hour doorman"= "doorman",
  "24 hour doorman"= "doorman",
  "24 hr doorman"= "doorman",
  "24/7 full-time doorman concierge" = "doorman",
  "twenty-four hour concierge and doorman" = "doorman",
  
  "part-time doorman"="part time doorman",
  
  "no fee!"="no fee",
  "no fee!!"="no fee",
  
  "gym/fitness" = "fitness center",
  "fitness room" = "fitness center",
  "state-of-the-art fitness center" = "fitness center",
  "fitness facility" = "fitness center",
  "fully-equipped club fitness center" = "fitness center",
  "state-of-the-art cardio and fitness club" = "fitness center",
  "health club"="fitness center",
    
  "prewar" = "pre-war",
  "pre war" = "pre-war",
  "pre-war charm" = "pre-war",
  
  "post-war" = "post war",
  "postwar" = "post war",
  
  "roof-deck" = "roof deck",
  "roofdeck" = "roof deck",
  "rooftopdeck" = "roof deck",
  
  "private roofdeck" = "private roof deck",
  "private roof-deck" = "private roof deck",
  
  "outdoor areas" = "outdoor space",

  "publicoutdoor"="common outdoor space",
  "building-common-outdoor-space"="common outdoor space",
  
  "private-outdoor-space" = "private outdoor space",
  
  "high-speed internet" = "high speed internet",
  "high speed internet available" = "high speed internet",
  
  "private-balcony" = "private balcony",
  "private-balcony" = "balcony",
  "private balcony" = "balcony",
  
  "pool" = "swimming pool",
  "indoor swimming pool" = "indoor pool",
  
  "brand new" = "new construction",
  "all new" = "new construction",
  
  "new renovation" = "newly renovated",
  
  "terraces / balconies" = "terrace",
  
  "garden/patio" = "garden",
  "residents garden" = "garden",
  "shared garden" = "garden",

  "wheelchair ramp"="wheelchair access",
  
  "decorative fireplace" = "fireplace",
  "fireplaces" = "fireplace",
  "wood-burning fireplace" = "fireplace",
  "deco fireplace" = "fireplace",
  "fire place" = "fireplace",
  "working fireplace" = "fireplace",
  
  "hi rise"="highrise",
  
  "on-site garage" = "garage",
  "common parking/garage" = "garage",
  "full service garage" = "garage",
  "on-site attended garage" = "garage",
  "garage attached" = "garage",
  "garage parking" = "garage",
  "garage." = "garage",
  
  "parking space"="parking",
  "valet parking"="parking",
  "on-site parking lot"="parking",
  "on-site parking"="parking",
  "assigned-parking-space"="parking",
  "assigned-parking-space"="parking",
  "on-site parking available"="parking",
  "parking available"="parking",
  
  "laundry in building"="common laundry",
  "laundry in unit" = "private laundry",
  "laundry room"="private laundry",
  "on-site laundry" = "common laundry",
  "laundry on floor" = "common laundry",
  "laundry & housekeeping" = "laundry",
  "private laundry room on every floor" = "common laundry",
  "laundry on every floor" = "common laundry",
  
  "multi level" = "multi-level",
  
  "high ceiling" = "high ceilings",
  
  "walk in closet(s)" = "walk in closet",
  "walk-in closet" = "walk in closet",
  
  "closets galore!" = "closet",
  "closet space" = "closet",
  "extra closet space" = "closet",
  "great closet space" = "closet",
  
  "marble bathroom"="marble bath",
  
  "stainless steel appliances" = "ss",
  "stainless steel" = "ss",
  "stainless appliances" = "ss",
  "stainless steal appliances" = "ss",
  "stainless steel kitchen" = "ss",
  
  "wifi access" = "wifi",
  
  "granite counter tops" = "granite kitchen",
  "granite countertops" = "granite kitchen",
  "granite counters" = "granite kitchen",
  "granite counter" = "granite kitchen",
  
  "close to subway" = "subway",
  
  "actual apt. photos"="photos",
  "actual photos!"="photos",
  
  "childrens playroom"="playroom",
  "children's playroom"="playroom",
  "playroom/nursery"="playroom",
  "children's playroom"="playroom",
  "children playroom"="playroom",
  "kids playroom"="playroom"
  
)

mapFeatures=function(list_of_features){
  output = list()
  for(f in list_of_features){
    f = tolower(f)
    mapped = FALSE
    
    # looking for exact matches
    if(f %in% freq_features){
      output[[length(output)+1]] = f
      mapped = TRUE
    }else{
      for(code in names(freq_features_map)){
        if(f == code){
          output[[length(output)+1]] = freq_features_map[[code]]
          mapped = TRUE
        }else{
          #no mapping for this feature
        }
      }
    }
    
    # checking if string contains any features
    if(!mapped){
      for(ff in freq_features){
        if(grepl(ff, f)){
          output[[length(output)+1]] = ff
          mapped=TRUE
        }
      }
      for(code in names(freq_features_map)){
        if(grepl(code,f)){
          output[[length(output)+1]] = freq_features_map[[code]]
          mapped = TRUE
        }
      }
    }
    
    if(!mapped){
      #print(f)
      if(str_length(f)>23){
        output[[length(output)+1]] = "long string"
      }else{
        #  some unmapped string
        output[[length(output)+1]] = f
      }
    }
  }
  unlist(output)
}
data$features_star = lapply(data$features,mapFeatures)

getRows=function(word){
  grep(word, tolower(data$features_star))
}
rows=dim(data)[1]
for(f in freq_features){
  print(f)
  data[f] = rep(0, rows)
  data[getRows(f),f] = 1
  data[f] = factor(data[[f]])
}

rows_test=dim(real_test)[1]
for(f in freq_features){
  print(f)
  real_test[f] = rep(0, rows_test)
  real_test[getRows(f),f] = 1
  real_test[f] = factor(real_test[[f]])
}


# building_id
if(FALSE){
  building_score = list()
  ids = unique(data$building_id)
  for(id in ids){
    count = sum(data$building_id==id)
    high_p = sum(data$building_id==id & data$interest_level=="high") / count
    med_p = sum(data$building_id==id & data$interest_level=="medium") / count 
    low_p = sum(data$building_id==id & data$interest_level=="low") / count
    building_score[[id]]=c(low_p, med_p, high_p)
  }
  data$b_low_score = sapply(data$building_id, FUN=function(b_id) building_score[[as.String(b_id)]][1])
  data$b_med_score = sapply(data$building_id, FUN=function(b_id) building_score[[as.String(b_id)]][2])
  data$b_high_score = sapply(data$building_id, FUN=function(b_id) building_score[[as.String(b_id)]][3])
  data$bldg_score = mapply(function(high,med) 2*high+med, data$b_high_score, data$b_med_score)
  
  sum(data$bldg_score==0)
  mean_score = mean(data[data$bldg_score!=0,]$bldg_score)
  data[data$bldg_score==0,"bldg_score"] = mean_score
  
  getBScore=function(id,index){
    #print(id)
    id = as.String(id)
    if(id %in% names(building_score)){
      building_score[[id]][index]
    }else{
      score=0
      score    
    }
  }
  real_test$b_low_score = sapply(real_test$building_id, FUN=function(b_id) getBScore(b_id,1))
  real_test$b_med_score = sapply(real_test$building_id, FUN=function(b_id) getBScore(b_id,2))
  real_test$b_high_score = sapply(real_test$building_id, FUN=function(b_id) getBScore(b_id,3))
  real_test$bldg_score = mapply(function(high,med) 2*high+med, real_test$b_high_score, real_test$b_med_score)
  real_test[real_test$bldg_score==0,"bldg_score"] = mean_score
  
  rm(building_score)
  rm(ids)
}

b_factors = getFactors(data$building_id, real_test$building_id)
data[,'building_id_int'] = as.numeric(factor(data$building_id, levels = b_factors))
real_test[,'building_id_int'] = as.numeric(factor(real_test$building_id, levels = b_factors))

data$building_id = factor(data$building_id)
real_test$building_id = factor(real_test$building_id)


## description
# kitchen
data$kitchen = mapply(grepl, pattern=c("kitchen"), x=tolower(data$description))
data$kitchen =factor(data$kitchen)

real_test$kitchen = mapply(grepl, pattern=c("kitchen"), x=tolower(real_test$description))
real_test$kitchen =factor(real_test$kitchen)


## address
addr_code = c("n","e","s","w","st","ave")
addr_expansion = c("north","east","south","west","street","avenue")
skip_words = c("laundry", "elevator", "bathroom", "kitchen", "usa", "bedroom", 
               "apartment", "amazing", "dryer", "price", "great", "renovated", 
               "location", "doorman", "fee", "studio", "call", "get", "exclusive",
               "deal", "it", "hot", "included", "bed", "today", "large", "newly",
               "your", "in", "the", "what", "stunning", "txt", "a", "steal", 
               "living", "kr", "room", "sized", "beautiful", "modern", "there",
               "own", "your", "throw", "big", "with", "k", "r", "huge", "lux",
               "told", "near", "massive", "views", "joanne", "now", "to", "stay",
               "asap", "size", "restaurants", "restaurant", "fantastic", "unique",
               "block", "fire", "ask", "for", "more", "and", "super", "hardwood", "over",
               "please", "read", "cannot", "fabulous", "broker", "only", "washer", "top",
               "best", "ultra", "service", "this", "is", "full", "eyes", "off", "miss",
               "updates", "that", "extra", "you", "heart", "decorative", "dplx", "grab",
               "perfect", "gone", "time", "true", "limited", "amenities", "years", "needs",
               "on", "sqft", "floors", "floor", "if", "out", "brokers", "our", "have", "years",
               "spacious", "outdoor", "space", "we", "us", "experience", "thousands", "take",
               "care", "find", "listings", "special", "sq", "ft", "brand", "new", "home",
               "utilities", "all", "one", "from", "delicious", "generous", "reno", "long",
               "bright", "charming", "bath", "roof", "deck", "plus", "office", "train",
               "share", "viewing", "place", "look", "finding", "fit", "ceilings","market",
               "priced", "just", "steps", "amex", "gift", "card", "fees", "spectacular",
               "private", "terrace", "penthouse", "luxury", "pool", "th", "free", "balcony",
               "gorgeous", "tempting", "br", "ba", "wow", "oh", "i", "will", "not", "last",
               "bathrooms", "no", "live", "like", "the", "it", "cost", "much", "nd", "pets",
               "ok", "st", "rd", "how", "before", "secure", "subway")

expandCodes=function(s){
  words = strsplit(s, " ")[[1]]
  output = c()
  total_words = length(words)
  skip_words_count = 0
  for(word in words){
    #print(word)
    index = match(word, addr_code)
    if(!is.na(index)){
      output = c(output,addr_expansion[index])
    }else{
      index = match(word, skip_words)
      if(is.na(index)){
        output = c(output,word)
      }else{
        # skip the word
        skip_words_count = skip_words_count + 1
      }
    }
  }
  if(total_words>0){
    per = skip_words_count / total_words
    #print(paste(per, skip_words_count, total_words))
    if(per>0.75){
      skip = "SKIP"
    }else{
      paste(output, collapse = " ")
    }
  }else{
    skip="SKIP"
  }
}

getNewColumn=function(df){
  x = df$display_address
  x = str_replace_all(x, "[^[:alnum:]]", " ")
  x = gsub("[[:digit:]]+", " ", str_trim(x))
  x = gsub("\\s+", " ", str_trim(x))
  x = tolower(x)
  length(unique(x))
  y = unlist(lapply(x, FUN=expandCodes))
  length(unique(y))
  y
  #factor(y)
}

data$display_addr = getNewColumn(data)
real_test$display_addr = getNewColumn(real_test)

addr_factors = getFactors(data$display_addr, real_test$display_addr)
data[,'display_addr_int'] = as.numeric(factor(data$display_addr, levels = addr_factors))
real_test[,'display_addr_int'] = as.numeric(factor(real_test$display_addr, levels = addr_factors))

data$display_addr = factor(data$display_addr)
real_test$display_addr = factor(real_test$display_addr)

as.data.frame(data[sample(1:rows,50),c("display_address","display_addr") ])


getWordCount=function(s){
  words = strsplit(s, " ")[[1]]
  length(words)
}

# mean price of neighborhood
# do mapping to improve accuracy
rm(mean)
all_ = rbind(data[,c("display_addr","price")], real_test[,c("display_addr","price")])
mean_addr_price = as.data.frame(aggregate(price~display_addr, all_, mean))
rm(all_)

getPriceDiff = function(addr, price){
  mean_price = mean_addr_price[mean_addr_price$display_addr==addr,"price"]
  price - mean_price
}
data$price_diff_daddr = mapply(function(addr, p) getPriceDiff(as.String(addr),p), data$display_addr, data$price)

real_test$price_diff_daddr = mapply(function(addr, p) getPriceDiff(as.String(addr),p), real_test$display_addr, real_test$price)



# xxx encoding for addr_expansion xxx
for(exp in addr_expansion){
  #exp = "east"
  data[,exp] = rep(0,dim(data)[1])
  data[,exp] = lapply(data[,"display_addr"], FUN=function(x) grepl(exp,x))
}

for(exp in addr_expansion){
  #exp = "east"
  real_test[,exp] = rep(0,dim(real_test)[1])
  real_test[,exp] = lapply(real_test[,"display_addr"], FUN=function(x) grepl(exp,x))
}

data[sample(1:rows,10),c("display_address","display_addr","street") ]



## 
x = c("bathrooms", "bedrooms", "rooms", ### "bathbed",  
      "log_price", "price_diff_daddr", "price_diff_bedrooms", "price_diff_rooms", 
      "bed_price", "room_price", #"price", 
      "nphotos", "kitchen", 
      
      "latitude", "longitude", 
      names(places),
      
      "f_len",
      freq_features ,
      
      # xxx addr_expansion, "east", "street",
      "display_addr",
      
      "manager_id", 
      ##"low_score","med_score","high_score"
      ##"mngr_skill",

      "building_id"
      ##"bldg_score"
      # xxx "low_bldg", "med_bldg", "high_bldg"
      )

y = c("interest_level")

x_y = c(x,y)

rows = dim(data)[1]

# OverSampling `high`
#data_save = data[,]
#data = rbind(data, data[data$interest_level=="high",])

if(TRUE){
  # without ensembling GBM
  train_rows = sample(1:rows, 0.80*rows, replace=F)
  train = data[train_rows, x_y]
  test = data[-train_rows, x_y]
  
  
  ### h2o initialization ###
  h2o.init(nthreads = -1, max_mem_size = "6G") 
  h2o_train = as.h2o(train)
  h2o_test = as.h2o(test)
  h2o_train$interest_level = as.factor(h2o_train$interest_level)
  h2o_test$interest_level = as.factor(h2o_test$interest_level)
  
  
  ## Training model
  gbm_clf <- h2o.gbm(x = x
                     ,y = y
                     ,training_frame = h2o_train
                     ,distribution = "multinomial"
                     ,stopping_metric = "logloss"
                     ,ntrees = 620
                     ,max_depth = length(x)
                     ,min_rows = 200
                     ,stopping_rounds = 10
                     ,learn_rate = 0.025
                     ,sample_rate = 0.5
                     ,col_sample_rate = 0.5
                     ,model_id = "gbm_31"
  )
  
  gbm_clf_pred = as.data.table(h2o.predict(gbm_clf, h2o_test))
  predictions = gbm_clf_pred$predict
  getAccuracy(predictions, as.factor(test$interest_level))
  
  id = unname(sapply(real_test$listing_id, `[[`, 1))
  real_test2 = real_test[, x]
  real_test_h2o = as.h2o(real_test2)
  gbm_clf_pred = as.data.table(h2o.predict(gbm_clf, real_test_h2o))
  
  to_write = data.frame("listing_id"=id, "high"=gbm_clf_pred$high, "medium"=gbm_clf_pred$medium, "low"=gbm_clf_pred$low)
  write.csv(to_write, file="gbm_xv.csv", row.names = FALSE, quote = FALSE)
 
  rm(train)
  rm(test)
  rm(gbm_clf_pred)
  rm(predictions)
  rm(id)
  rm(real_test2)
  rm(real_test_h2o)
  rm(to_write)
  h2o.shutdown()
  
  # as.data.frame(h2o.varimp(gbm_clf))
}

# sort( sapply(ls(),function(x){object.size(get(x))})) 
if(TRUE){
  
  # ensemble of GBMs
  h2o.init(nthreads = -1, max_mem_size = "6G") 
  
  train = data[, x_y]
  h2o_train = as.h2o(train)
  h2o_train$interest_level = as.factor(h2o_train$interest_level)
  
  id = unname(sapply(real_test$listing_id, `[[`, 1))
  h2o_test = as.h2o(real_test[, x])
    
  
  ## Level - 1
  # glm
  glm_model <- h2o.glm(x = x
                     ,y = y
                     ,training_frame = h2o_train
                     #,nfolds = 5L
                     ,seed = 1
                     ,keep_cross_validation_predictions = TRUE
                     ,family = 'multinomial'
                     ,alpha = 0.7
                     ,lambda_search = TRUE
                     #,standardize = T
  )
  h2o.varimp(glm_model)
  
  #glm_train = as.data.table(h2o.cross_validation_holdout_predictions(glm_model))
  glm_train = as.data.table(h2o.predicti(glm_model, h2o_train))
  glm_train = data.table(listing_id=data$listing_id, 
                          gl_high = glm_train$high, gl_low = glm_train$low, gl_medium = glm_train$medium)
  write.csv(glm_train, file="lvl-1_glm_train.csv", row.names = FALSE)
  #glm_train = read.csv(file="lvl-1_glm_train.csv")
  
  glm_test = as.data.table(h2o.predict(glm_model, h2o_test))
  glm_test = data.table(listing_id = id, 
                         gl_high = glm_test$high, gl_low = glm_test$low, gl_medium = glm_test$medium)
  write.csv(glm_test, file="lvl-1_glm_test.csv", row.names = FALSE)
  #glm_test = read.csv(file="lvl-1_glm_test.csv")
  
  
  # DeepLearning
  dl_model <- h2o.deeplearning(x = x
                             ,y = y
                             ,training_frame = h2o_train
                             #,nfolds = 5L
                             ,keep_cross_validation_predictions = TRUE
                             ,variable_importances = TRUE
                             ,hidden = c(100,100)
                             ,l2 = 0.0001
  )
  h2o.varimp_plot(dl_model)
  
  #dl_train <- as.data.table(h2o.cross_validation_holdout_predictions(dl_model))
  dl_train <- as.data.table(h2o.predict(dl_model, h2o_train))
  dl_train <- data.table(listing_id = data$listing_id, 
                         dl_high = dl_train$high, dl_low = dl_train$low, dl_medium = dl_train$medium)
  write.csv(dl_train, file="lvl-1_dl_train.csv", row.names = FALSE)
  #dl_train = read.csv(file="lvl-1_dl_train.csv")
  
  dl_test <- as.data.table(h2o.predict(dl_model, h2o_test))
  dl_test <- data.table(listing_id = id, 
                        dl_high = dl_test$high, dl_low = dl_test$low, dl_medium = dl_test$medium)
  write.csv(dl_test, file="lvl-1_dl_test.csv", row.names = FALSE)
  #dl_test = read.csv(file="lvl-1_dl_test.csv")
  
  
  # Random Forest
  rf_model = h2o.randomForest(x = x
                             ,y = y
                             ,training_frame = h2o_train
                             #,keep_cross_validation_predictions = TRUE
                             #,nfolds = 5L
                             ,ntrees = 600
                             ,max_depth = length(x)
                             ,min_rows = 200
                             ,sample_rate = 0.5
                             )
  h2o.varimp_plot(rf_model)
  
  #rf_train = as.data.table(h2o.cross_validation_holdout_predictions(rf_model))
  rf_train = as.data.table(h2o.predict(rf_model, h2o_train))
  rf_train = data.table(listing_id = data$listing_id, 
                         rf_high = rf_train$high, rf_low=rf_train$low, rf_medium=rf_train$medium)
  write.csv(rf_train, file="lvl-1_rf_train.csv", row.names = FALSE)
  # rf_train = read.csv(file="lvl-1_rf_train.csv")
  
  rf_test = as.data.table(h2o.predict(rf_model, h2o_test))
  rf_test = data.table(listing_id = id, 
                        rf_high = rf_test$high, rf_low = rf_test$low, rf_medium = rf_test$medium)
  write.csv(rf_test, file="lvl-1_rf_test.csv", row.names = FALSE)
  # rf_test = read.csv(file="lvl-1_rf_test.csv")
  
  ## GBM
  gbm_model = h2o.gbm(x = x
                     ,y = y
                     ,training_frame = h2o_train
                     #,nfolds = 5L
                     #,keep_cross_validation_predictions = TRUE
                     ,ntrees = 600
                     ,max_depth = length(x)
                     ,min_rows = 200
                     ,stopping_rounds = 10
                     ,learn_rate = 0.025
                     ,sample_rate = 0.5
                     ,col_sample_rate = 0.5
                     )
  
  h2o.varimp_plot(gbm_model)
  
  # gbm_train = as.data.table(h2o.cross_validation_holdout_predictions(gbm_model))
  gbm_train = as.data.table(h2o.predict(gbm_model, h2o_train))
  gbm_train = data.table(listing_id = data$listing_id, 
                          gbm_high = gbm_train$high, gbm_low=gbm_train$low, gbm_medium = gbm_train$medium)
  write.csv(gbm_train, file="lvl-1_gbm_train.csv", row.names = FALSE)
  # gbm_train = read.csv(file="lvl-1_gbm_train.csv")
  
  gbm_test = as.data.table(h2o.predict(gbm_model, h2o_test))
  gbm_test = data.table(listing_id = id,
                         gbm_high = gbm_test$high, gbm_low = gbm_test$low, gbm_medium = gbm_test$medium)
  write.csv(gbm_test, file="lvl-1_gbm_test.csv", row.names = FALSE)
  # gbm_test = read.csv(file="lvl-1_gbm_test.csv")
  
  
  ## Level - 2
  if(FALSE){
    lvl2_train = data.table(listing_id = data$listing_id, interest_level = data$interest_level)
    lvl2_test = data.table(listing_id = id)
    
    lvl2_train = glm_train[lvl2_train, on='listing_id']
    lvl2_test = glm_test[lvl2_test, on='listing_id']
    
    lvl2_train <- dl_train[lvl2_train,on='listing_id']
    lvl2_test <- dl_test[lvl2_test,on='listing_id']
    
    lvl2_train <- rf_train[lvl2_train, on='listing_id']
    lvl2_test <- rf_test[lvl2_test, on='listing_id']
    
    lvl2_train <- gbm_train[lvl2_train, on='listing_id']
    lvl2_test <- gbm_test[lvl2_test, on='listing_id']
  }  
  
  x2 = c("bathrooms", "bedrooms", "rooms", ### "bathbed",  
        "log_price", "price_diff_daddr", "price_diff_bedrooms", "price_diff_rooms", 
        "bed_price", "room_price", #"price", 
        "nphotos",
        "latitude", "longitude", 
        names(places),
        "f_len",
        freq_features ,
        "display_addr_int",
        "manager_id_int", 
        "building_id_int"
  )
  y = c("interest_level")
  x2_y = c(x2,y)
  
  lvl2_train = data[,c(x2_y,"listing_id")]
  lvl2_test = real_test[,c(x2,"listing_id")]

  for(f in freq_features){
    print(f)
    lvl2_train[,f] = as.integer(unlist(data[,f]))
    lvl2_test[,f] = as.integer(unlist(real_test[,f]))
  }
    
  for(f in c("manager_id","building_id","display_addr")){
    print(f)
    lvl2_train[,f] = as.integer(unlist(data[,f]))
    lvl2_test[,f] = as.integer(unlist(real_test[,f]))
  }
  
  ## Taking outputs of lvl-2
  lvl2_train = cbind(lvl2_train, glm_train[,!(names(glm_train) %in% c("listing_id"))])
  lvl2_test = cbind(lvl2_test, glm_test[,!(names(glm_test) %in% c("listing_id"))])
  
  lvl2_train = cbind(lvl2_train, dl_train[,!(names(glm_train) %in% c("listing_id"))])
  lvl2_test = cbind(lvl2_test, dl_test[,!(names(glm_test) %in% c("listing_id"))])
  
  lvl2_train = cbind(lvl2_train, rf_train[,!(names(glm_train) %in% c("listing_id"))])
  lvl2_test = cbind(lvl2_test, rf_test[,!(names(glm_test) %in% c("listing_id"))])
  
  lvl2_train = cbind(lvl2_train, gbm_train[,!(names(glm_train) %in% c("listing_id"))])
  lvl2_test = cbind(lvl2_test, gbm_test[,!(names(glm_test) %in% c("listing_id"))])
  
  #write.csv(lvl2_train,"lvl-2_train.csv", row.names = FALSE)
  #write.csv(lvl2_test,"lvl-2_test.csv", row.names = FALSE)
  
  
  y = as.integer(factor(data$interest_level))
  y = y - 1
  
  d_train = xgb.DMatrix(data=as.matrix(
    lvl2_train[,!(names(lvl2_train) %in% c('listing_id','interest_level'))]), label=y)
  
  d_test = xgb.DMatrix(data = as.matrix(
    lvl2_test[,!(names(lvl2_test) %in% c('listing_id','interest_level'))])) 
  
  
  ## Level - 2 xgboost without CV and Looping
  kfolds = 10
  folds = createFolds(y, k = kfolds, list = TRUE, returnTrain = FALSE)
  fold = as.numeric(unlist(folds[1]))
  
  train_one <- xgb.DMatrix(data = as.matrix(
    lvl2_train[,!(names(lvl2_train) %in% c('listing_id','interest_level'))]), label=y[])
  val_one <- xgb.DMatrix(data = as.matrix(
    lvl2_train[fold,!(names(lvl2_train) %in% c('listing_id','interest_level'))]), label=y[fold])
  
  seed=1234
  set.seed(seed)
  xgb_params = list(
    colsample_bytree= 0.5,
    subsample = 0.5,
    eta = 0.025,
    objective= 'multi:softprob',
    max_depth= as.integer(length(x2)/2),
    min_child_weight= 100,
    eval_metric= "mlogloss",
    num_class = 3,
    seed = seed
  )
  
  xgb_model = xgb.train(params = xgb_params,
                        data = train_one,
                        nrounds = 800,
                        watchlist = list(val = val_one),
                        print_every_n = 25,
                        early_stopping_rounds=50)
  # val-mlogloss = 0.264 with lvl-1    min_rows=200
  # val-mlogloss = 0.286 with lvl-1    min_rows=10
  # val-mlogloss = 0.266 with lvl-1    min_rows=100
  
  # val-mlogloss = 0.569 withOUT lvl-1 min_rows=200
  # val-mlogloss = 0.562 withOUT lvl-1 min_rows=10
  # val-mlogloss = 0.556 withOUT lvl-1 min_rows=50
  # val-mlogloss = 0.557 withOUT lvl-1 min_rows=100 :)
  
  imp = xgb.importance(names(d_train), model=xgb_model)
  xgb.ggplot.importance(imp)
  
  xgb_pred <- as.data.table(t(matrix(predict(xgb_model, d_test), nrow=3, ncol=nrow(d_test))))
  names(xgb_pred) = c("high","low","medium")
  xgb_pred <- data.table(listing_id=lvl2_test$listing_id, xgb_pred[,list(high,medium,low)])
  
  write.csv(xgb_pred, file="lvl-2_xgb.csv", row.names = FALSE, quote = FALSE)

  
  
  ## Level - 2 xgboost with CV and Looping
  kfolds = 10
  folds = createFolds(y, k = kfolds, list = TRUE, returnTrain = FALSE)
  
  
  all_class = {}
  for (param_1 in c(1:5)){
    
    #fold = as.numeric(unlist(folds[param_1]))
    #train_one <- xgb.DMatrix(data = as.matrix(
    #  lvl2_train[-fold,!(names(lvl2_train) %in% c('listing_id','interest_level'))]), label=y[-fold])
    #val_one <- xgb.DMatrix(data = as.matrix(
    #  lvl2_train[fold,!(names(lvl2_train) %in% c('listing_id','interest_level'))]), label=y[fold])
    
    #print (param_1)
    for (param_2 in 1:4){
      #print (param_2)
      param <- list(  objective           = "multi:softprob", 
                      num_class           = 3,
                      #max_delta_step=8,
                      booster             = "gbtree",
                      eta                 = 0.025,
                      max_depth           = as.integer(length(x2)/param_2),
                      #alpha=32,
                      min_child_weight    = 100,
                      subsample           = 0.8,
                      colsample_bytree    = 0.50
      )
      
      print(paste("Now training the model with",param_1,"and",param_2))
      set.seed(param_1)
      clf2 <- xgb.train(   params              = param, 
                           data                = d_train,
                           nrounds             = 800, 
                           verbose             = 0,
                           early_stopping_rounds  = 20,
                           watchlist           = list(val=d_val),
                           maximize            = FALSE,
                           eval_metric         = "mlogloss"
      )
      print(clf2$best_score)
      
      pred_exp2 = t(matrix(predict(clf2, d_test), nrow=3, ncol=nrow(d_test)))
      colnames(pred_exp2) <- c("high","low","medium")
      #print(head(all_class))
      all_class = cbind(all_class,pred_exp2)
    }
  }
  all_class = as.data.table(all_class)
  max_col = dim(all_class)[2]
  
  j_high = all_class[,seq(1,max_col,3),with=F]
  j_low = all_class[,seq(2,max_col,3),with=F]
  j_medium = all_class[,seq(3,max_col,3),with=F]
    
  j_high[,high_mean := rowMeans(.SD)]
  j_low[,low_mean := rowMeans(.SD)]
  j_medium[,medium_mean := rowMeans(.SD)]
  
  submit <- data.table(listing_id = lvl2_test$listing_id, 
                       high = j_high$high_mean, medium = j_medium$medium_mean, low = j_low$low_mean)
  fwrite(submit, "stack_bagging_xgboost.csv")
  
  h2o.shutdown()
}
