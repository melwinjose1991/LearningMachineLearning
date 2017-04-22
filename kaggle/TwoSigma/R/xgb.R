library(jsonlite)
library(magrittr)
library(dplyr)
library(purrr)
library(data.table)
library(stringr)
library(tm)
library(ggmap)
library(xgboost)
library(lightgbm)
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
if(TRUE){
  manager_score = list()
  ids = unique(data$manager_id)
  for(id in ids){
    count = sum(data$manager_id==id)
    high_p = sum(data$manager_id==id & data$interest_level=="high") / count
    med_p = sum(data$manager_id==id & data$interest_level=="medium") / count 
    low_p = sum(data$manager_id==id & data$interest_level=="low") / count
    manager_score[[id]]=c(low_p, med_p, high_p)
  }
  data$low_score = sapply(data$manager_id, FUN=function(m_id) manager_score[[m_id]][1])
  data$med_score = sapply(data$manager_id, FUN=function(m_id) manager_score[[m_id]][2])
  data$high_score = sapply(data$manager_id, FUN=function(m_id) manager_score[[m_id]][3])
  data$mngr_skill = mapply(function(med,high) 2*high+med, data$high_score, data$med_score)
  
  getScore=function(id,index){
    #print(id)
    if(id %in% names(manager_score)){
      manager_score[[id]][index]
    }else{
      score=0
      score    
    }
  }
  real_test$low_score = sapply(real_test$manager_id, FUN=function(m_id) getScore(m_id,1))
  real_test$med_score = sapply(real_test$manager_id, FUN=function(m_id) getScore(m_id,2))
  real_test$high_score = sapply(real_test$manager_id, FUN=function(m_id) getScore(m_id,3))
  real_test$mngr_skill = mapply(function(med,high) 2*high+med, real_test$high_score, real_test$med_score)
  
  rm(manager_score)
  rm(ids)
}

data$manager_id = as.integer(factor(data$manager_id))
real_test$manager_id = as.integer(factor(real_test$manager_id))


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
  "pre-war" #"post-war",
  #"reduced fee",
  
  ##"loft",
  #"closet","walk in closet",
  #"marble bath",
  ##"ss", #stainless steel kitchen
  #"wifi",
  #"green building",
  #"granite kitchen",
  #"subway",
  #"cats allowed",
  #"dogs allowed",
  #"furnished"
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
  #"dishwasher"
  ##"fitness center"
  
  #"photos",
  #"playroom",
  
  #"long string"
)
## Adding a new feature has to have a accuracy better
## than 0.70 with x=freq_features only and better
## varaible importance than 25


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
  
  #"private-balcony" = "private balcony",
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
  
  #"hi rise"="highrise",
  
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
  
  #"walk in closet(s)" = "walk in closet",
  #"walk-in closet" = "walk in closet",
  
  #"closets galore!" = "closet",
  #"closet space" = "closet",
  #"extra closet space" = "closet",
  #"great closet space" = "closet",
  
  #"marble bathroom"="marble bath",
  
  "stainless steel appliances" = "ss",
  "stainless steel" = "ss",
  "stainless appliances" = "ss",
  "stainless steal appliances" = "ss",
  "stainless steel kitchen" = "ss"
  
  #"wifi access" = "wifi",
  
  #"granite counter tops" = "granite kitchen",
  #"granite countertops" = "granite kitchen",
  #"granite counters" = "granite kitchen",
  #"granite counter" = "granite kitchen",
  
  #"close to subway" = "subway",
  
  #"actual apt. photos"="photos",
  #"actual photos!"="photos",
  
  #"childrens playroom"="playroom",
  #"children's playroom"="playroom",
  #"playroom/nursery"="playroom",
  #"children's playroom"="playroom",
  #"children playroom"="playroom",
  #"kids playroom"="playroom"
  
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
  #data[f] = factor(data[[f]])
}

rows_test=dim(real_test)[1]
for(f in freq_features){
  print(f)
  real_test[f] = rep(0, rows_test)
  real_test[getRows(f),f] = 1
  #real_test[f] = factor(real_test[[f]])
}


# building_id
data$building_id = as.integer(factor(data$building_id))
real_test$building_id = as.integer(factor(real_test$building_id))


## description
# kitchen
data$kitchen = mapply(grepl, pattern=c("kitchen"), x=tolower(data$description))
#data$kitchen =factor(data$kitchen)

real_test$kitchen = mapply(grepl, pattern=c("kitchen"), x=tolower(real_test$description))
#real_test$kitchen =factor(real_test$kitchen)


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
data$display_addr = as.integer(factor(data$display_addr))

real_test$display_addr = getNewColumn(real_test)
real_test$display_addr = as.integer(factor(real_test$display_addr))

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
x_cols = c("bathrooms", "bedrooms", "rooms", "bathbed",  
      "log_price", "price_diff_daddr", "price_diff_bedrooms", "price_diff_rooms", 
      "bed_price", "room_price", #"price", 
      "nphotos", #"kitchen", 
      
      "latitude", "longitude", 
      names(places),
      
      "f_len",
      freq_features,
      
      # xxx addr_expansion, "east", "street",
      "display_addr",
      
      "manager_id",
      ##"low_score","med_score","high_score"
      #"mngr_skill",

      "building_id"
      # xxx "low_bldg", "med_bldg", "high_bldg"
      )

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
  
  imp = xgb.importance(names(x_train), model=xgb_model)
  xgb.ggplot.importance(imp)
  
  all_predictions =  (as.data.frame(matrix(predict(xgb_model, dtest), nrow=dim(dtest), byrow=TRUE)))
}

trainAndTestLGBM=function(nrounds=800, depth=8, leaves=200,  col_sample=0.5, 
                          row_sample=0.5, learn_rate=0.025, seed=1234, ltrain, lval, ltest){
  set.seed(seed)
  lxgb_params = list( objective = "multiclass", metric  = "multi_logloss",
                      learning_rate = learn_rate, 
                      num_leaves=leaves, max_depth = depth,  
                      bagging_fraction = row_sample, bagging_freq=1, bagging_seed = seed,
                      feature_fraction = col_sample, feature_fraction_seed = seed)
  
  lxgb_model = lgb.train(params=lxgb_params, data=ltrain, nrounds=nrounds, 
                valids=list(test = lval), 
                varbose=1, eval_freq = 50, num_class=3)
  
  all_predictions = as.data.frame(predict(lxgb_model, data=ltest, reshape = TRUE))
}

if(TRUE){
  y = as.integer(factor(data$interest_level))
  y = y - 1
  
  #create folds
  kfolds = 10
  folds = createFolds(y, k = kfolds, list = TRUE, returnTrain = FALSE)
  fold = as.numeric(unlist(folds[1]))
  
  x_train = data[-fold, x_cols] #Train set
  x_val = data[fold, x_cols]    #Out of fold validation set
  
  y_train = y[-fold]
  y_val = y[fold]
  
  
  #convert to xgbmatrix
  dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
  dval = xgb.DMatrix(as.matrix(x_val), label=y_val)
  dtest = xgb.DMatrix(data.matrix(real_test[, x_cols]))

  ltrain = lgb.Dataset(as.matrix(x_train), label=y_train)
  lval = lgb.Dataset(as.matrix(x_val), label=y_val)
  ltest = lgb.Dataset(as.matrix(real_test[, x_cols]))
    
  # training XGB
  depth = as.integer(length(x_cols)/2)
  nrounds = 800
  pred_1 = trainAndTestXGB(nrounds=nrounds, depth=depth, seed=1234, dtrain=dtrain, dval=dval, dtest=dtest)
  pred_2 = trainAndTestXGB(nrounds=nrounds, depth=depth, seed=2345, dtrain=dtrain, dval=dval, dtest=dtest)
  pred_3 = trainAndTestXGB(nrounds=nrounds, depth=depth, seed=3456, dtrain=dtrain, dval=dval, dtest=dtest)
  stack_pred_xgb = (pred_1 + pred_2 + pred_3)/3
  stack_pred_xgb = cbind(stack_pred_xgb, real_test$listing_id)
  names(stack_pred_xgb) = c("high","low","medium","listing_id")
  stack_pred_xgb = stack_pred_xgb[,c(1,3,2,4)]
  head(stack_pred_xgb)
  
  file_name = paste0("xgboost_", "x",as.String(length(x_cols)), "_d",depth, "_r",nrounds, ".csv")
  write.csv(stack_pred_lxgb, file_name, row.names = FALSE)
  
  # training LGBM
  leaves = as.integer(length(x_cols)*0.5)
  pred_4 = trainAndTestLGBM(nrounds=nrounds, depth=depth, leaves=leaves, seed=1234, 
                            ltrain=ltrain, lval=lval, ltest=as.matrix(real_test[, x_cols]))
  pred_5 = trainAndTestLGBM(nrounds=nrounds, depth=depth, leaves=leaves, seed=2345, 
                            ltrain=ltrain, lval=lval, ltest=as.matrix(real_test[, x_cols]))
  pred_6 = trainAndTestLGBM(nrounds=nrounds, depth=depth, leaves=leaves, seed=3456, 
                            ltrain=ltrain, lval=lval, ltest=as.matrix(real_test[, x_cols]))
  stack_pred_lxgb = (pred_4 + pred_5 + pred_6)/3
  stack_pred_lxgb = cbind(stack_pred_lxgb, real_test$listing_id)
  names(stack_pred_lxgb) = c("high","low","medium","listing_id")
  stack_pred_lxgb = stack_pred_lxgb[,c(1,3,2,4)]
  head(stack_pred_lxgb)
  
  file_name = paste0("xgbl_", "x",as.String(length(x_cols)), "_l",leaves, "_r",nrounds, ".csv")
  write.csv(stack_pred_lxgb, file_name, row.names = FALSE)
  
}
