#gene classification (grouping of the same genes together)

library(readxl)
library(tidyverse)
library(caret)
library(C50)
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
# 0.8722	0.8705	0.9648

index <- createDataPartition(dataClass$Class, p=0.75, list=FALSE)
# accuracy	kappa	auroc
# 0.8738	0.8721	0.9677

index <- createDataPartition(dataClass$Class, p=0.80, list=FALSE)
# accuracy	kappa	auroc
# 0.8697	0.8679	0.9675

index <- createDataPartition(dataClass$Class, p=0.85, list=FALSE)
# accuracy	kappa	auroc
# 0.8815	0.8799	0.9736

index <- createDataPartition(dataClass$Class, p=0.90, list=FALSE)
# accuracy	kappa	auroc
# 0.8791	0.8774	0.9688

index <- createDataPartition(dataClass$Class, p=0.95, list=FALSE)
# accuracy	kappa	auroc
# 0.8959	0.8943	0.9742

#split into train and test data
dataClass.training <- dataClass[index,]
dataClass.test <- dataClass[-index,]

x <- select(dataClass.training, -Class) #predictor
y <- dataClass.training$Class #response

#parallel computing
myCl <- makeCluster(detectCores())
registerDoParallel(myCl)

model <- C5.0(x,y) #model training

predicted <- predict(model, dataClass.test) #factor of predicted classes
predictedProb <- predict(model, dataClass.test, type = "prob") #probability of predicted classes

confusionMatrix(dataClass.test$Class, predicted) #confusion matrix

multiclass.roc(dataClass.test$Class,predictedProb) #auroc value

stopCluster(myCl) #stop parallel computing
