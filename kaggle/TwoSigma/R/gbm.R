library(jsonlite)
library(magrittr)
library(dplyr)
library(purrr)
library(h2o)
library(data.table)
library(stringr)
library(tm)
library(ggmap)

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


# total rooms
data$rooms = data$bathrooms + data$bedrooms
data = data[!data$rooms>=10, ]

real_test$rooms = real_test$bathrooms + real_test$bedrooms


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

plot(data[,c("longitude","latitude")])
  # removing outliers
#quantile(data$latitude, probs=seq(0,1,by=0.00125))
#sum(data$latitude<=39.58 | data$latitude>=41.88)
data = data[!(data$latitude<=39.58 | data$latitude>=41.88), ]

quantile(data$longitude , probs=seq(0,1,by=0.00125))
sum(data$longitude<=-74.23 | data$longitude>=-73.60)
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
real_test$nphotos = lengths(real_test$photos)


# manager_id
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
  "elevator",
  "hardwood floors",
  ### "cats allowed",
  ### "dogs allowed",
  "doorman", ##"24hr doorman","part time doorman",
  ### "dishwasher",
  "laundry", "common laundry", ### "private laundry",
  "no fee",
  ##"fitness center",
  "pre-war", #"post-war",
  ##"roof deck",
  ##"outdoor space", "common outdoor space", "private outdoor space",
  ##"dining room",
  ##"high speed internet",
  ##"balcony", "private balcony",
  ##"swimming pool",
  ##"new construction", "newly renovated",
  #"terrace",
  ##"exclusive",
  #"loft",
  #"garden","common garden","private garden",
  #"wheelchair access",
  #"fireplace",
  ##"simplex",
  #"lowrise","highrise","midrise",
  #"garage", "common garage",
  "reduced fee"
  ##"furnished",
  #"multi-level",
  #"high ceilings"
  
  ## in top 50 of high
  ##"loft",
  #"closet","walk in closet",
  #"marble bath",
  ##"ss" #stainless steel kitchen
  #"wifi"
  
  ## in top 50 of medium
  #"green building",
  #"granite kitchen",
  #"subway"
)
## Adding a new feature has to have a accuracy better
## than 0.70 with x=freq_features only and better
## varaible importance than 25


freq_features_map = c(
  "hardwood"="hardwood floors",
  "hardwood floor"="hardwood floors",
  "hardwood flooring"="hardwood floors",
  
  "full-time doorman"="24hr doorman",
  "ft doorman" = "24hr doorman",
  "24/7 doorman"= "24hr doorman",
  "24/7 doorman concierge"= "24h doorman",
  "24-hour doorman"= "24hr doorman",
  "24 hour doorman"= "24hr doorman",
  "24 hr doorman"= "24hr doorman",
  "24/7 full-time doorman concierge" = "24hr doorman",
  "twenty-four hour concierge and doorman" = "24hr doorman",
  
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
  
  "pool" = "swimming pool",
  "indoor swimming pool" = "indoor pool",
  
  "brand new" = "new construction",
  "all new" = "new construction",
  
  "new renovation" = "newly renovated",
  
  "terraces / balconies" = "terrace",
  
  "garden/patio" = "garden",
  "residents garden" = "common garden",
  "shared garden" = "common garden",

  "wheelchair ramp"="wheelchair access",
  
  "decorative fireplace" = "fireplace",
  "fireplaces" = "fireplace",
  "wood-burning fireplace" = "fireplace",
  "deco fireplace" = "fireplace",
  "fire place" = "fireplace",
  "working fireplace" = "fireplace",
  
  "hi rise"="highrise",
  
  "on-site garage" = "garage",
  "common parking/garage" = "common garage",
  "full service garage" = "garage",
  "on-site attended garage" = "garage",
  "garage attached" = "garage",
  "garage parking" = "garage",
  "garage." = "garage",
  
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
  
  "close to subway" = "subway"
)

mapFeatures=function(list_of_features){
  output = list()
  for(f in list_of_features){
    f = tolower(f)
    if(f %in% freq_features){
      output[[length(output)+1]] = f
    }else{
      for(code in names(freq_features_map)){
        if(f == code){
          output[[length(output)+1]] = freq_features_map[[code]]
        }else{
          #non-mapped features
        }
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
data$display_addr = factor(data$display_addr)

real_test$display_addr = getNewColumn(real_test)
real_test$display_addr = factor(real_test$display_addr)

as.data.frame(data[sample(1:rows,50),c("display_address","display_addr") ])


getWordCount=function(s){
  words = strsplit(s, " ")[[1]]
  length(words)
}

# mean price of neighborhood
# do mapping to improve accuracy

all_ = rbind(data[,c("display_addr","price")], real_test[,c("display_addr","price")])
mean_addr_price = as.data.frame(aggregate(price~display_addr, all_, mean))
rm(all_)

getPriceDiff = function(addr, price){
  mean_price = mean_addr_price[mean_addr_price$display_addr==addr,"price"]
  price - mean_price
}
data$mean_price_diff = mapply(function(addr, p) getPriceDiff(as.String(addr),p), data$display_addr, data$price)

real_test$mean_price_diff = mapply(function(addr, p) getPriceDiff(as.String(addr),p), real_test$display_addr, real_test$price)



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
x = c("bathrooms", "bedrooms", "price", "log_price", ### "bathbed",  
      "mean_price_diff",
      "f_len", "manager_id", "rooms", 
      "nphotos", "bed_price", "room_price", "kitchen", 
      
      "latitude", "longitude", 
      names(places),
      
      freq_features ,
      
      # xxx addr_expansion, "east", "street",
      "display_addr",
      
      "building_id"
      # xxx "low_bldg", "med_bldg", "high_bldg"
      )

y = c("interest_level")

x_y = c(x,y)

rows = dim(data)[1]
train_rows = sample(1:rows, 0.75*rows, replace=F)
train = data[, x_y]
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
                   ,sample_rate = 0.45
                   ,col_sample_rate = 0.45
                   ,model_id = "gbm_31"
)

gbm_clf_pred = as.data.table(h2o.predict(gbm_clf, h2o_test))
predictions = gbm_clf_pred$predict
getAccuracy(predictions, as.factor(test$interest_level))


### Change Logs ###
# outliers+log_pr 620   200   0.758   0.xxx   0.xxx
# rmvd exp+safe   625   200   0.xxx   0.861   0.601
# lowest 4 remvd  600   200   0.xxx   0.868   0.600 -(park_central, univ_pace, xpnsv_madison, safe_carnegie_hill, univ_city_college)
# mean_price_diff 600   200   0.751   0.865   0.599
# expensive+safe  600   200   0.xxx   0.xxx   0.599 
# lowest 5 rmvd   600   200   0.749   0.874   0.600 subway_grand_central, univ_yesh, univ_city_college, univ_cuny, subway_herald_square
# more univ       600   200   0.749   0.863   0.600 vars=47, places=25
# lowest 5 rmvd   600   200   0.751   0.869   0.600 lowest features: private laundry, 
#                                                     bathbed, dishwasher, cats, dogs   


### >>> TRAIN ON ALL THE TRAINING DATA and then proceed <<< ### 
## Test Data
id = unname(sapply(real_test$listing_id, `[[`, 1))

real_test2 = real_test[, x]

real_test_h2o = as.h2o(real_test2)
gbm_clf_pred = as.data.table(h2o.predict(gbm_clf, real_test_h2o))

to_write = data.frame("listing_id"=id, "high"=gbm_clf_pred$high, "medium"=gbm_clf_pred$medium, "low"=gbm_clf_pred$low)
write.csv(to_write, file="gbm_xv.csv", row.names = FALSE, quote = FALSE)

