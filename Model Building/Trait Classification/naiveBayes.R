#trait classification (classification of related traits for each gene)

library(readxl)
library(tidyverse)
library(caret)
library(naivebayes)
library(doParallel)
library(pROC)

trainingData <- read_excel("Training Data/trainingData.xlsx")

dataTrait <- trainingData %>%
  select(-c(OsID,Class)) %>% #exclude OsID and Class column
  mutate_all(~replace(., is.na(.), 2)) #replace NA as 2
  
col_names <- names(dataTrait[,c(2:3,5:21)]) #exclude log2 fold change and pcc columns (numeric)
dataTrait[col_names] <- lapply(dataTrait[col_names],factor) #convert the rest of column to factor

set.seed(1234)

#train-test data partition split
index <- createDataPartition(dataTrait$Trait, p=0.70, list=FALSE)
# accuracy	kappa	auroc
# 0.4483	0.2969	0.7798

index <- createDataPartition(dataTrait$Trait, p=0.75, list=FALSE)
# accuracy	kappa	auroc
# 0.4509	0.2983	0.7784

index <- createDataPartition(dataTrait$Trait, p=0.80, list=FALSE)
# accuracy	kappa	auroc
# 0.4556	0.3053	0.7861

index <- createDataPartition(dataTrait$Trait, p=0.85, list=FALSE)
# accuracy	kappa	auroc
# 0.4566	0.3051	0.793

index <- createDataPartition(dataTrait$Trait, p=0.90, list=FALSE)
# accuracy	kappa	auroc
# 0.4579	0.3077	0.7818

index <- createDataPartition(dataTrait$Trait, p=0.95, list=FALSE)
# accuracy	kappa	auroc
# 0.4537	0.3012	0.7883

#split into train and test data
dataTrait.training <- dataTrait[index,]
dataTrait.test <- dataTrait[-index,]

x <- select(dataTrait.training, -Trait) #predictor
y <- dataTrait.training$Trait #response

fitControl <- trainControl(
  method = "repeatedcv", #cross validation
  number = 10, #10-fold 
  repeats = 5, #repeated 5 times
  allowParallel = T) 

#parallel computing
myCl <- makeCluster(detectCores())
registerDoParallel(myCl)

#model training
model <- train(x,y,
              method = "naive_bayes",
              trControl = fitControl)

print(model)

predicted <- predict(model, dataTrait.test) #factor of predicted traits
predictedProb <- predict(model, dataTrait.test, type = "prob") #probability of predicted traits

confusionMatrix(dataTrait.test$Trait, predicted, mode = "everything") #confusion matrix

multiclass.roc(dataTrait.test$Trait,predictedProb) #auroc value

stopCluster(myCl) #stop parallel computing