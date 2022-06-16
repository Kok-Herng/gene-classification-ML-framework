#gene classification (grouping of the same genes together)

library(readxl)
library(tidyverse)
library(caret)
library(naivebayes)
library(doParallel)
library(pROC)

trainingData <- read_excel("trainingData.xlsx")

dataClass <- trainingData %>%
  group_by(OsID) %>%
  mutate(Count=n()) %>% #count number of occurrence for each OsID
  ungroup() %>%
  filter(Count>=10) %>% #use only OsID with 10 or more occurrences
  select(-c(Class, Count, Trait)) %>% #exclude Class, Count and Trait columns
  mutate(Class = as.integer(fct_inorder(OsID))) %>% #group same OsID together and assign them a class
  select(-OsID) %>% #exclude OsID column
  mutate_all(~replace(., is.na(.), 2)) #replace NA as 2 for EV columns

col_names <- names(dataClass[,c(2:3,5:21)]) #exclude log2 fold change and pcc columns (numeric)
dataClass[col_names] <- lapply(dataClass[col_names],as.factor) #convert the rest of columns to factor

set.seed(1234)

#train-test data partition split
index <- createDataPartition(dataClass$Class, p=0.70, list=FALSE)
# accuracy	kappa	auroc
# 0.807	0.8043	0.9877

index <- createDataPartition(dataClass$Class, p=0.75, list=FALSE)
# accuracy	kappa	auroc
# 0.8087	0.806	0.9875

index <- createDataPartition(dataClass$Class, p=0.80, list=FALSE)
# accuracy	kappa	auroc
# 0.8056	0.8029	0.9875

index <- createDataPartition(dataClass$Class, p=0.85, list=FALSE)
# accuracy	kappa	auroc
# 0.8128	0.8101	0.9879

index <- createDataPartition(dataClass$Class, p=0.90, list=FALSE)
# accuracy	kappa	auroc
# 0.8142	0.8115	0.9874  

index <- createDataPartition(dataClass$Class, p=0.95, list=FALSE)
# accuracy	kappa	auroc
# 0.8231	0.8203	0.9872

#split into train and test data
dataClass.training <- dataClass[index,]
dataClass.test <- dataClass[-index,]

x <- select(dataClass.training, -Class) #predictor
y <- dataClass.training$Class #response

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

predicted <- predict(model, dataClass.test) #factor of predicted classes
predictedProb <- predict(model, dataClass.test, type = "prob") #probability of predicted classes

confusionMatrix(dataClass.test$Class, predicted) #confusion matrix

multiclass.roc(dataClass.test$Class,predictedProb) #auroc value

stopCluster(myCl) #stop parallel computing
