library(stringr)
library(tm)

missing_version = "1"

train_fe = read.csv(paste0("data/train_",missing_version,".csv"), header=TRUE, sep=",")
test_fe = read.csv(paste0("data/test_",missing_version,".csv"), header=TRUE, sep=",")

train_goal = read.csv(paste0("data/train_goal.csv"), header=TRUE, sep=",")
test_goal = read.csv(paste0("data/test_goal.csv"), header=TRUE, sep=",")

train_fe = cbind(train_fe, train_goal["goal_star"])
test_fe = cbind(test_fe, test_goal["goal_star"])



## Base Features

## name : string

## desc : string

## goal : numerical $$$

## keywords : single unique string with `-`

## disable_communication - categorical : (YES/NO)

## country - categorical

## currency - categorical

## deadline - numeric

## status_changed_at : numeric time

## created_at : numeric time

## launched_at : numeric time



## Engineered Features

## desc_<word>
stop_words = c("the", "if", "was", "every", "while", "ever", "any", 
               "together", "would", "use", "going", "between", "for",
               "a", "and", "to", "of", "in", "is", "for", "with", "",
               "that", "an", "your", "on", "you", "we", "this", "by",
               "from", "it", "will", "are", "be", "about", "all", "i",
               "your", "you", "their", "at", "as", "can", "my", "our",
               "have", "us", "who", "get", "or", "into", "his", "its", 
               "through", "but", "me", "what", "where", "her", "could",
               "do", "dont", "each", "during", "has", "he", "him", "how",
               "im", "must", "only", "other", "put", "she", "so","some",
               "something", "than","them","there","these","they","want",
               "wants", "were", "when", "which", "without", "yet",
               "0", "1", "2", "3", "4", "5", "6", "7", "8", "9","1st",
               "10", "20", "100","12","also","always","am", "allows","around",
               "available", "back", "become", "been", "before", "behind",
               "both", "bring", "brings", "bringing", "called", "cant",
               "come", "comes", "different", "follow", "following", "follows",
               "found", "give", "giving", "goes", "got", "had", "happen",
               "here", "inside", "ive", "just", "keep", "know", "let", "lets",
               "like", "made", "make", "making", "more", "most", "need", "needed",
               "needs", "others", "out", "over", "own", "same" ,"self", "since",
               "take", "taking", "tell", "then", "things", "those", "too",
               "try", "turn", "under", "very", "within", "yourself", "why",
               "again", "against", "after", "anyone", "anything", "anywhere",
               "another", "away", "being", "used", "using", "thats",
               
               "one", "two", "three", "first", "second", "third", "four", "five")

stem_map = list("adventures"="adventure", "childrens"="children", "comics"="comic", "created"="create",
                "creating"="create", "days"="day", "designed"="design", "designs"="design", "events"="event",
                "explores"="explore", "exploring"="explore", "features"="feature", "friends"="friend",
                "funding"="fund", "funds"="fund", "games"="game", "handcrated"="handmade", "helping"="help",
                "helps"="help", "house"="home", "learning"="learn", "looking"="look", "meets"="meet", 
                "photos"="photo", "printed"="print", "prints"="print", "products"="product", "projects"="project",
                "recorded"="record", "recording"="record", "releasing"="release", "shows"="show", "songs"="song",
                "videos"="video", "women"="woman", "worlds"="world", "writing"="write", "written"="write",
                "drawing"="draw", "arts"="art", "artists"="artist", "artwork"="art", "books"="book", "cards"="card",
                "dreams"="dream", "paintings"="painting", "students"="student")

splitAndClean = function(value, split_by="\\s+"){
  #value = train_fe[1,"desc"]
  #split_by = "\\s+"
  
  x = as.character(value)
  words = tolower(unique(unlist(strsplit(x,split_by))))
  words = unlist(lapply(words, function(w) gsub("[^[:alnum:]]","",w)))
  words = words[!words %in% stop_words]
  #words = sapply(words, function(x) ifelse(x %in% names(stem_map), stem_map[[x]], x))
  words
}

splitCleanJoin = function(value, split_by="\\s+"){
  #value = train_fe$desc[1]
  # split_by="\\s+"
  
  #x = as.character(value)
  #words = tolower(unique(unlist(strsplit(x,split_by))))
  #words = unlist(lapply(words, function(w) gsub("[^[:alnum:]]","",w)))
  #words = words[!words %in% stop_words]
  
  words = splitAndClean(value, split_by)
  paste(words, collapse = " ")
}

splitCount = function(value, split_by="\\s+", search){
  #value = train_fe$desc[1]
  #split_by="\\s+" 
  
  x = as.character(value)
  #words = tolower(unique(unlist(strsplit(x,split_by))))
  words = unlist(strsplit(x,split_by))
  #words = unlist(lapply(words, function(w) gsub("[^[:alnum:]]","",w)))
  sum(grepl(search, words))
}

#train_fe$desc_star = sapply(train_fe$desc, splitCleanJoin)
#test_fe$desc_star = sapply(test_fe$desc, splitCleanJoin)

getDiffKeywords = function(col_name){
  #col_name = "desc"  
  
  words_1_list = sort(table(unlist(sapply(train_fe[train_fe$final_status==1, col_name], 
                                   function(x) splitAndClean(x) ))), decreasing=TRUE)

  words_0_list = sort(table(unlist(sapply(train_fe[train_fe$final_status==0, col_name], 
                                   function(x) splitAndClean(x) ))), decreasing=TRUE)

  words_one = names(words_1_list[words_1_list>(0.01*sum(train_fe$final_status==1))])
  words_zero = names(words_0_list[words_0_list>(0.01*sum(train_fe$final_status==0))])

  #words_one = names(words_1_list)
  #words_zero = names(words_0_list)
  
  #only_one = setdiff(words_one, words_zero)
  #only_zero = setdiff(words_zero, words_one)

  n_ones = sum(train_fe$final_status==1)
  n_zeros = sum(train_fe$final_status==0)
  
  filtered_one_words = names(words_1_list[only_one])[sapply(words_1_list[only_one], as.numeric) > 5]
  filtered_zero_words = names(words_0_list[only_zero])[sapply(words_0_list[only_zero], as.numeric) > 5]
  
  words = c(filtered_one_words, filtered_zero_words)
  words
  
}

getKeywords = function(col_name, min_count=1000, split_by="\\s+"){
  #col_name = "keywords"  
  #min_count = 1000
  #split_by="-"
  
  df = c(as.character(train_fe[,col_name]), as.character(test_fe[,col_name]))
  words_list = sort(table(unlist(sapply(df, function(x) splitAndClean(x,split_by) ))), decreasing=TRUE)
  length(words_list)
  
  filtered_words = names(words_list)[sapply(words_list, as.numeric) > min_count]
  length(filtered_words)
  
  sort(filtered_words)
}

getTopNKeywords = function(df, col_name, N=100, split_by="\\s+"){
  #df = train_fe[1:100,]
  #col_name = "desc"  
  #N = 100
  #split_by="\\s+"
  
  column_str = as.character(df[,col_name])
  words_list = sort(table(unlist(sapply(column_str, function(x) splitAndClean(x,split_by) ))), decreasing=TRUE)
  length(words_list)
 
  names(words_list[1:N])
}

# OneHotEncoding of words desc
#words = getKeywords("desc", min_count = 500)
train_words = getTopNKeywords(train_fe, "desc", N=400, split_by="\\s+")
test_words = getTopNKeywords(test_fe, "desc", N=400, split_by="\\s+")
words = sort(union(train_words, test_words))
length(words)
train_fe$desc_starstar = sapply(train_fe$desc, function(x) splitCleanJoin(x,"\\s+"))
test_fe$desc_starstar = sapply(test_fe$desc, function(x) splitCleanJoin(x,"\\s+"))
for(word in words){

  col_name = paste0("desc_",word)
  print(paste0("Checking for ", word, " Column:", col_name))
  word = paste0("\\<",word,"\\>")
  train_fe[,col_name] = sapply(train_fe$desc_starstar, function(x) ifelse(grepl(word, x), 1, 0))
  test_fe[,col_name] = sapply(test_fe$desc_starstar, function(x) ifelse(grepl(word, x), 1, 0))
  
  #col_name = paste0("desc_count_",word)
  #print(paste0("Checking for ", word, " Column:", col_name))
  #word = paste0("\\<",word,"\\>")
  #train_fe[,col_name] = sapply(train_fe$desc_starstar, function(x) splitCount(x, "\\s+", word))
  #test_fe[,col_name] = sapply(test_fe$desc_starstar, function(x) splitCount(x, "\\s+", word))
  
}
#desc_words_cols = paste0("desc_", words)
desc_words_cols = paste0("desc_count_", words)
train_fe$desc_useful = str_length(train_fe$desc_starstar) / str_length(train_fe$desc) 
test_fe$desc_useful = str_length(test_fe$desc_starstar) / str_length(test_fe$desc) 



# OneHotEncoding of words in keywords
#words = getKeywords("keywords", min_count = 500, split_by = "-")
#train_words = getTopNKeywords(train_fe, "keywords", N=400, split_by="-")
#test_words = getTopNKeywords(test_fe, "keywords", N=400, split_by="-")
#words = sort(union(train_words, test_words))
#length(words)
train_fe$keywords_starstar = sapply(train_fe$keywords, function(x) splitCleanJoin(x,"-"))
test_fe$keywords_starstar = sapply(test_fe$keywords, function(x) splitCleanJoin(x,"-"))
if(FALSE){
  
  col_name = paste0("keywords_",word)
  print(paste0("Checking for ", word, " Column:", col_name))
  word = paste0("\\<",word,"\\>")
  train_fe[,col_name] = sapply(train_fe$keywords_starstar, function(x) ifelse(grepl(word, x), 1, 0))
  test_fe[,col_name] = sapply(test_fe$keywords_starstar, function(x) ifelse(grepl(word, x), 1, 0))
  
  #col_name = paste0("keywords_count_",word)
  #print(paste0("Checking for ", word, " Column:", col_name))
  #word = paste0("\\<",word,"\\>")  
  #train_fe[,col_name] = sapply(train_fe$keywords_starstar, function(x) splitCount(x, "-", word))
  #test_fe[,col_name] = sapply(test_fe$keywords_starstar, function(x) splitCount(x, "-", word))
  
}
#keywords_words_cols = paste0("keywords_", words)
#keywords_words_cols = paste0("keywords_count_", words)
train_fe$keywords_useful = str_length(train_fe$keywords_starstar) / str_length(train_fe$keywords) 
test_fe$keywords_useful = str_length(test_fe$keywords_starstar) / str_length(test_fe$keywords) 


# name_length
train_fe$name_length = sapply(train_fe$name, function(x) nchar(as.character(x)))
test_fe$name_length = sapply(test_fe$name, function(x) nchar(as.character(x)))

# desc_length
train_fe$desc_length = sapply(train_fe$desc,function(x) nchar(as.character(x)))
test_fe$desc_length = sapply(test_fe$desc,function(x) nchar(as.character(x)))

# keywords_length
train_fe$keywords_length = sapply(train_fe$keywords, function(x) nchar(as.character(x)))
test_fe$keywords_length = sapply(test_fe$keywords, function(x) nchar(as.character(x)))

# dashes
train_fe$dashes = sapply(train_fe$keywords, function(x) str_count(as.character(x,"-")) )
test_fe$dashes = sapply(test_fe$keywords, function(x) str_count(as.character(x,"-")) )



## deadline - created_at
sum(train_fe$deadline > train_fe$created_at) == dim(train_fe)[1]
train_fe$deadline_created_in_sec = train_fe$deadline - train_fe$created_at
train_fe$deadline_created_in_min = train_fe$deadline_created_in_sec/60
train_fe$deadline_created_in_hour = train_fe$deadline_created_in_sec/(60*60)
train_fe$deadline_created_in_day = train_fe$deadline_created_in_sec/(60*60*24)

sum(test_fe$deadline > test_fe$created_at) == dim(test_fe)[1]
test_fe$deadline_created_in_sec = test_fe$deadline - test_fe$created_at
test_fe$deadline_created_in_min = test_fe$deadline_created_in_sec/60
test_fe$deadline_created_in_hour = test_fe$deadline_created_in_sec/(60*60)
test_fe$deadline_created_in_day = test_fe$deadline_created_in_sec/(60*60*24)

## deadline - launched_at
sum(train_fe$deadline > train_fe$launched_at) == dim(train_fe)[1]
train_fe$deadline_launched_in_sec = train_fe$deadline - train_fe$launched_at
train_fe$deadline_launched_in_min = train_fe$deadline_launched_in_sec / (60)
train_fe$deadline_launched_in_hour = train_fe$deadline_launched_in_sec / (60*60)
train_fe$deadline_launched_in_day = train_fe$deadline_launched_in_sec / (60*60*24)

sum(test_fe$deadline > test_fe$launched_at) == dim(test_fe)[1]
test_fe$deadline_launched_in_sec = test_fe$deadline - test_fe$launched_at
test_fe$deadline_launched_in_min = test_fe$deadline_launched_in_sec / (60)
test_fe$deadline_launched_in_hour = test_fe$deadline_launched_in_sec / (60*60)
test_fe$deadline_launched_in_day = test_fe$deadline_launched_in_sec / (60*60*24)

## deadline - status_changed
sum(train_fe$deadline > train_fe$state_changed_at) == dim(train_fe)[1]
train_fe$deadline_state_changed_in_sec = train_fe$deadline - train_fe$state_changed_at
train_fe$deadline_state_changed_in_min = train_fe$deadline_state_changed_in_sec / (60)
train_fe$deadline_state_changed_in_hour = train_fe$deadline_state_changed_in_sec / (60*60)
train_fe$deadline_state_changed_in_day = train_fe$deadline_state_changed_in_sec / (60*60*24)

sum(test_fe$deadline > test_fe$state_changed_at) == dim(test_fe)[1]
test_fe$deadline_state_changed_in_sec = test_fe$deadline - test_fe$state_changed_at
test_fe$deadline_state_changed_in_min = test_fe$deadline_state_changed_in_sec / (60)
test_fe$deadline_state_changed_in_hour = test_fe$deadline_state_changed_in_sec / (60*60)
test_fe$deadline_state_changed_in_day = test_fe$deadline_state_changed_in_sec / (60*60*24)

## log transformations
# goal_star
plot(hist(train_fe$goal_star))
plot(hist(log(train_fe$goal_star)))
train_fe$goal_star_log = log(train_fe$goal_star)

plot(hist(test_fe$goal_star))
plot(hist(log(test_fe$goal_star)))
test_fe$goal_star_log = log(test_fe$goal_star)

# deadline_created_in_sec
plot(hist(train_fe$deadline_created_in_sec))
plot(hist(log(train_fe$deadline_created_in_sec)))
train_fe$deadline_created_in_sec_log = log(train_fe$deadline_created_in_sec)

plot(hist(test_fe$deadline_created_in_sec))
plot(hist(log(test_fe$deadline_created_in_sec)))
test_fe$deadline_created_in_sec_log = log(test_fe$deadline_created_in_sec)

# deadline_launched_in_sec
plot(hist(train_fe$deadline_launched_in_sec))
plot(hist(log(train_fe$deadline_launched_in_sec)))

# deadline_state_changed_in_sec
plot(hist(train_fe$deadline_state_changed_in_sec))
plot(hist(log(train_fe$deadline_state_changed_in_sec)))

# launched_at - created_at
sum(train_fe$launched_at>train_fe$created_at) == dim(train_fe)[1]
train_fe$launch_created_in_sec = train_fe$launched_at - train_fe$created_at
plot(hist(train_fe$launch_created_in_sec))
plot(hist(log(train_fe$launch_created_in_sec)))
train_fe$launch_created_in_sec_log = log(train_fe$launch_created_in_sec)

sum(test_fe$launched_at>test_fe$created_at) == dim(test_fe)[1]
test_fe$launch_created_in_sec = test_fe$launched_at - test_fe$created_at
plot(hist(test_fe$launch_created_in_sec))
plot(hist(log(test_fe$launch_created_in_sec)))
test_fe$launch_created_in_sec_log = log(test_fe$launch_created_in_sec)

## state_changed after deadline
train_fe$state_change_after_deadline = sapply(train_fe$deadline_state_changed_in_sec, function(x){
  if(x>0){
    0
  }else{
    1
  }
})
train_fe$state_change_after_deadline = as.factor(train_fe$state_change_after_deadline)

test_fe$state_change_after_deadline = sapply(test_fe$deadline_state_changed_in_sec, function(x){
  if(x>0){
    0
  }else{
    1
  }
})
test_fe$state_change_after_deadline = as.factor(test_fe$state_change_after_deadline)

## month year
getDay = function(date_numeric){
  #date_numeric = train_fe$deadline[1]
  as.numeric(format(as.Date(as.POSIXct(date_numeric, origin="1970-01-01")), "%d"))
}

getMonth = function(date_numeric){
  #date_numeric = train_fe$deadline[1]
  as.numeric(format(as.Date(as.POSIXct(date_numeric, origin="1970-01-01")), "%m"))
}

getYear = function(date_numeric){
  #date_numeric = train_fe$deadline[1]
  as.numeric(format(as.Date(as.POSIXct(date_numeric, origin="1970-01-01")), "%Y"))
}

for(col in c("deadline","state_changed_at","created_at","launched_at")){

  print(paste0("Now at train ", col))
  train_fe[,paste0(col,"_day")] = sapply(train_fe[,col], function(x) getDay(x))
  train_fe[,paste0(col,"_month")] = sapply(train_fe[,col], function(x) getMonth(x))
  train_fe[,paste0(col,"_year")] = sapply(train_fe[,col], function(x) getYear(x))

  print(paste0("Now at test ", col))
  test_fe[,paste0(col,"_day")] = sapply(test_fe[,col], function(x) getDay(x))
  test_fe[,paste0(col,"_month")] = sapply(test_fe[,col], function(x) getMonth(x))
  test_fe[,paste0(col,"_year")] = sapply(test_fe[,col], function(x) getYear(x))

}



eng_version = "2"
write.csv(train_fe, paste0("data/train_",missing_version, "_", eng_version, ".csv"), row.names = FALSE)
write.csv(test_fe, paste0("data/test_",missing_version, "_", eng_version, ".csv"), row.names = FALSE)



eng_version = "???"
cols = c(desc_words_cols,"final_status")
write.csv(train_fe[,cols], paste0("data/train_",missing_version, "_", eng_version, ".csv"), row.names = FALSE)
write.csv(test_fe[,desc_words_cols], paste0("data/test_",missing_version, "_", eng_version, ".csv"), row.names = FALSE)
