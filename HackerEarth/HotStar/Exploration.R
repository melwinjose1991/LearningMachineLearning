library(data.table)
library(jsonlite)

#### Train
train <- fromJSON("data/train_data.json")

train_data <- data.table(ID = unlist(names(train)))
train_data[, `:=` (genres = unlist(lapply(train, '[',1)),
                   titles = unlist(lapply(train, '[',2)),
                   cities = unlist(lapply(train, '[', 3)),
                   segment = unlist(lapply(train, '[',4)),
                   dow = unlist(lapply(train, '[',5)),
                   tod = unlist(lapply(train, '[', 6))
)]


#### Test 
test <- fromJSON("data/test_data.json")

test_data <- data.table(ID  = unlist(names(test)))
test_data[,`:=` (genres = unlist(lapply(test, '[',1)),
                 titles = unlist(lapply(test, '[',2)),
                 tod = unlist(lapply(test, '[', 3)),
                 cities = unlist(lapply(test, '[',4)),
                 dow = unlist(lapply(test, '[',5))
)]



## Cities
head(train_data$cities)

getCities=function(df1,df2){
  #df1=train_data[1:10,]
  
  cities_map = list()
  all_cities_str = unlist(df1$cities)
  all_cities_str = c(all_cities_str, unlist(df1$cities))
  
  for(cities_str in all_cities_str){
    splits = unlist(strsplit(cities_str,","))
    for(split in splits){
      key_value = unlist(strsplit(split,":"))
      if(key_value[1] %in% names(cities_map)){
        #print(paste0("OLD:",key_value[1]))
        old_value = cities_map[[key_value[1]]]
        cities_map[key_value[1]]=old_value+1
      }else{
        #print(paste0("NEW:",key_value[1]))
        cities_map[key_value[1]]=1
      }
    }
  }
  cities_map
}

getOneHotEncoding=function(value){
  #features_name = cities_name
  value = train_data[100,]$cities
  
  print(cities_name)
  vec = rep(as.numeric(0),length(cities_name))
  splits = unlist(strsplit(value,","))
  for(split in splits){
    key_value = unlist(strsplit(split,":"))
    vec[which(cities_name==key_value[1])]=as.numeric(key_value[2])
  }
  print(vec)
  vec
}

cities_map = getCities(train_data, test_data)
cities_name = cities_map[sapply(cities_map, function(x) x>10000)]
length(cities_name)
#cities_name = sort(names(cities_map))

y = train_data[1:10,]
y[,cities_name] = lapply(y$cities, FUN=function(x) getOneHotEncoding(x))

y[,cities_name := getOneHotEncoding(cities)]
