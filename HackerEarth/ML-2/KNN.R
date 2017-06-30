library(class)



version = "1"
train = read.csv(paste0("data/train_",version, "_", version, ".csv"), header=TRUE, sep=",")
test = read.csv(paste0("data/test_",version, "_", version, ".csv"), header=TRUE, sep=",")

names(train)



## X
numerical_vars = c("state_changed_at","created_at","launched_at","deadline_created",
                   "deadline_launched","deadline_state_changed","goal_star",
                   "name_length", "desc_length", "keywords_length", "dashes")



## Hyper Parameter tunning
train_std_x = scale(train[,numerical_vars])

k=5
set.seed(1)
folds = sample(1:k, nrow(train_std_x), replace=TRUE)

#no_of_neighbors = sort(sample(1:500, 10))
no_of_neighbors = c(250)
cv.accuracies = matrix(NA, k, length(no_of_neighbors))

for(i in 1:k){
  
  for(neighbors in no_of_neighbors){
    
    knn_pred = knn(train_std_x[folds!=i,], train_std_x[folds==i,], train[folds!=i,"final_status"], 
                   k=neighbors)
    
    accuracy = mean(knn_pred==train[folds==i,"final_status"])
    
    cv.accuracies[i, match(neighbors, no_of_neighbors)] = accuracy
    
  }
  
}

mean.cv.accuracies = apply(cv.accuracies, 2, mean)
plot(no_of_neighbors, mean.cv.accuracies)



## Training & Prediction
test_std_x = scale(test[,numerical_vars])
knn_pred = knn(train_std_x, test_std_x, train$final_status, k=250)

#  25 : 0.669 : 0.649
# 100 : 0.678 : 0.658
# 250 : 0.681 : 0.658

knn_pred_df = data.frame(project_id=test$project_id, final_status=knn_pred)
write.csv(knn_pred_df, "submission_knn.csv", row.names = FALSE, quote=FALSE)
