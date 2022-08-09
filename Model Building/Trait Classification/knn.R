#trait classification (classification of related traits for each gene)

library(readxl)
library(tidyverse)
library(caret)
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
# 0.4622	0.3035	0.8845

index <- createDataPartition(dataTrait$Trait, p=0.75, list=FALSE)
# accuracy	kappa	auroc
# 0.44	0.274	0.8787

index <- createDataPartition(dataTrait$Trait, p=0.80, list=FALSE)
# accuracy	kappa	auroc
# 0.4327	0.2654	0.8785

index <- createDataPartition(dataTrait$Trait, p=0.85, list=FALSE)
# accuracy	kappa	auroc
# 0.4162	0.2439	0.873

index <- createDataPartition(dataTrait$Trait, p=0.90, list=FALSE)
# accuracy	kappa	auroc
# 0.3945	0.2141	0.8793

index <- createDataPartition(dataTrait$Trait, p=0.95, list=FALSE)
# accuracy	kappa	auroc
# 0.3612	0.172	0.8826

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
              method = "knn",
              trControl = fitControl)

print(model)

predicted <- predict(model, dataTrait.test) #factor of predicted traits
predictedProb <- predict(model, dataTrait.test, type = "prob") #probability of predicted traits

confusionMatrix(dataTrait.test$Trait, predicted, mode = "everything") #confusion matrix

multiclass.roc(dataTrait.test$Trait,predictedProb) #auroc value

stopCluster(myCl) #stop parallel computing