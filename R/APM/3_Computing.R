library(AppliedPredictiveModeling)

data("segmentationOriginal")

segData = subset(segmentationOriginal, Case=="Train")

cellID = segData$Cell
class = segData$Class
case = segData$Case
segData = segData[, -(1:3)]

statusColNum = grep("Status", names(segData))
statusColNum
segData = segData[, -statusColNum]



### Transformations ###
library(e1071)
library(MASS)
library(caret)

# skewness
# http://www.r-tutor.com/elementary-statistics/numerical-measures/skewness
skewness(segData$AngleCh1)
skewValues = apply(segData, 2, skewness)
head(skewValues)

plot(density(segData$AreaCh1))
AreaCh1_transformed_obj = BoxCoxTrans(segData$AreaCh1)
AreaCh1_transformed = predict(AreaCh1_transformed_obj, segData$AreaCh1)
head(AreaCh1_transformed)
plot(density(AreaCh1_transformed))

# PCA
pcaObj = prcomp(segData, center=TRUE, scale=TRUE)
percent_var = pcaObj$sdev^2 / sum(pcaObj$sdev^2) * 100
head(percent_var)

head(pcaObj$x[,1:5])

# spatialSign(segData)
# impute.knn

trans = preProcess(segData, method=c("BoxCox","center","scale","pca"))
trans

transformed = predict(trans, segData)
head(transformed[,1:5])

# transformation > centering > scaling > imputation > 
# feature extraction > spatial sign

