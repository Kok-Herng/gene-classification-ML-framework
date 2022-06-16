#gene classification (grouping of the same genes together)

library(readxl)
library(tidyverse)
library(caret)
library(ranger)
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
# 0.7799	0.7765	0.9882

index <- createDataPartition(dataClass$Class, p=0.75, list=FALSE)
# accuracy	kappa	auroc
# 0.7844	0.7811	0.9881

index <- createDataPartition(dataClass$Class, p=0.80, list=FALSE)
# accuracy	kappa	auroc
# 0.7826	0.7793	0.9875

index <- createDataPartition(dataClass$Class, p=0.85, list=FALSE)
# accuracy	kappa	auroc
#0.7868	0.7835	0.9883

index <- createDataPartition(dataClass$Class, p=0.90, list=FALSE)
# accuracy	kappa	auroc
# 0.7842	0.7808	0.9882

index <- createDataPartition(dataClass$Class, p=0.95, list=FALSE)
# accuracy	kappa	auroc
# 0.8034	0.8001	0.9865

#split into train and test data
dataClass.training <- dataClass[index,]
dataClass.test <- dataClass[-index,]

#parallel computing
myCl <- makeCluster(detectCores())
registerDoParallel(myCl)

#building model with factor as outcome
modelRes <- ranger(
  dependent.variable.name = "Class",
  data = dataClass.training,
)

#building model with probability as outcome
modelProb <- ranger(
  dependent.variable.name = "Class",
  data = dataClass.training,
  probability = T
)

predictedRes <- predict(modelRes, dataClass.test) #factor of predicted classes
predictedProb <- predict(modelProb, dataClass.test) #probability of predicted classes

confusionMatrix(dataClass.test$Class, predictedRes$predictions) #confusion matrix

multiclass.roc(dataClass.test$Class, predictedProb$predictions) #auroc value

stopCluster(myCl) #stop parallel computing
