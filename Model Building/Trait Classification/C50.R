#trait classification (classification of related traits for each gene)

library(readxl)
library(tidyverse)
library(caret)
library(C50)
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
# 0.5545	0.4329	0.9055

index <- createDataPartition(dataTrait$Trait, p=0.75, list=FALSE)
# accuracy	kappa	auroc
# 0.5496	0.4205	0.9

index <- createDataPartition(dataTrait$Trait, p=0.80, list=FALSE)
# accuracy	kappa	auroc
# 0.5549	0.4255	0.9065

index <- createDataPartition(dataTrait$Trait, p=0.85, list=FALSE)
# accuracy	kappa	auroc
# 0.5514	0.4181	0.9131

index <- createDataPartition(dataTrait$Trait, p=0.90, list=FALSE)
# accuracy	kappa	auroc
# 0.554	0.416	0.8989

index <- createDataPartition(dataTrait$Trait, p=0.95, list=FALSE)
# accuracy	kappa	auroc
# 0.5515	0.4217	0.9003

#split into train and test data
dataTrait.training <- dataTrait[index,]
dataTrait.test <- dataTrait[-index,]

x <- select(dataTrait.training, -Trait) #predictor
y <- dataTrait.training$Trait #response

#parallel computing
myCl <- makeCluster(detectCores())
registerDoParallel(myCl)

model <- C5.0(x,y) #model training

predicted <- predict(model, dataTrait.test) #factor of predicted traits
predictedProb <- predict(model, dataTrait.test, type = "prob") #probability of predicted traits

confusionMatrix(dataTrait.test$Trait, predicted, mode = "everything") #confusion matrix

multiclass.roc(dataTrait.test$Trait,predictedProb) #auroc value

stopCluster(myCl) #stop parallel computing