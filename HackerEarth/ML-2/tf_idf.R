library(tm)



## Reading Data
missing_version = "1"
eng_version = "2"
train = read.csv(paste0("data/train_",missing_version, "_", eng_version, ".csv"), header=TRUE, sep=",")
test = read.csv(paste0("data/test_",missing_version, "_", eng_version, ".csv"), header=TRUE, sep=",")

names(train)



## Combining Train and Test
descs = c(sapply(train$desc_starstar, as.character), 
          as.character(test$desc_starstar, as.character))

corpus = Corpus(VectorSource(descs))
corpus
inspect(corpus[2])



# tf-idf 
dtm_tf_idf = DocumentTermMatrix(corpus, control=list(weighting=weightTfIdf))
dtm_tf_idf = removeSparseTerms(dtm_tf_idf, 0.997)
dtm_tf_idf
inspect(dtm_tf_idf[3,])

no_of_docs = dim(train)[1]+dim(test)[1]
no_of_words = length(dtm_tf_idf$dimnames$Terms)

print(no_of_docs)
dim(train)[1]+dim(test)[1] == no_of_docs

tf_idfs = matrix(0, no_of_docs, no_of_words)
for(i in 1:no_of_docs){
  row = dtm_tf_idf[i,]
  vec = rep(0,no_of_words)
  vec[row$j] = row$v
  
  tf_idfs[i,] = vec
  
  if(i%%10000==0){
    print(paste0("#",i))
  }
}
tf_idf_sum = rowSums(tf_idfs)


# Writing td-idf to data frame
len_train = dim(train)[1]
len_test = dim(test)[1]

train_tfidf_df = data.frame(project_id=train$project_id, 
                               tfidf_sum=tf_idfs[1:len_train])
test_tfidf_df = data.frame(project_id=test$project_id, 
                              tfidf_sum=tf_idfs[len_train+1:len_test])

dim(train_tfidf_df)[1] == dim(train)[1]
dim(test_tfidf_df)[1] == dim(test)[1]

write.csv(train_tfidf_df, "train_tfidf.csv", row.names=FALSE, quote=FALSE)
write.csv(test_tfidf_df, "test_tfidf.csv", row.names=FALSE, quote=FALSE)
