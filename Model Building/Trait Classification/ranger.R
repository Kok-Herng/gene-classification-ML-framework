#trait classification (classification of related traits for each gene)

library(readxl)
library(tidyverse)
library(caret)
library(ranger)
library(doParallel)
library(pROC)

trainingData <- read_excel("trainingData.xlsx")

dataTrait <- trainingData %>%
  select(-c(OsID,Class)) %>% #exclude OsID and Class columns
  mutate_all(~replace(., is.na(.), 2)) #replace NA as 2 for all EV columns

col_names <- names(dataTrait[,c(2:3,5:21)]) #exclude log2 fold change and pcc columns (numeric)
dataTrait[col_names] <- lapply(dataTrait[col_names],factor) #convert the rest of column to factor

set.seed(1234)

#train-test data partition split
index <- createDataPartition(dataTrait$Trait, p=0.70, list=FALSE)
# accuracy	kappa	auroc
# 0.5432	0.408	0.9124

index <- createDataPartition(dataTrait$Trait, p=0.75, list=FALSE)
# accuracy	kappa	auroc
# 0.5302	0.3849	0.9088

index <- createDataPartition(dataTrait$Trait, p=0.80, list=FALSE)
# accuracy	kappa	auroc
# 0.5448	0.4038	0.9132

index <- createDataPartition(dataTrait$Trait, p=0.85, list=FALSE)
# accuracy	kappa	auroc
# 0.5371	0.3923	0.9121

index <- createDataPartition(dataTrait$Trait, p=0.90, list=FALSE)
# accuracy	kappa	auroc
# 0.5368	0.3886	0.9104

index <- createDataPartition(dataTrait$Trait, p=0.95, list=FALSE)
# accuracy	kappa	auroc
# 0.5091	0.3549	0.9122

#split into train and test data
dataTrait.training <- dataTrait[index,]
dataTrait.test <- dataTrait[-index,]

#parallel computing
myCl <- makeCluster(detectCores())
registerDoParallel(myCl)

#building model with factor as outcome
modelRes <- ranger(
  dependent.variable.name = "Trait",
  data = dataTrait.training
)

#building model with probability as outcome
modelProb <- ranger(
  dependent.variable.name = "Trait",
  data = dataTrait.training,
  probability = T
)

predictedRes <- predict(modelRes, dataTrait.test) #factor of predicted traits
predictedProb <- predict(modelProb, dataTrait.test) #probability of predicted traits

confusionMatrix(dataTrait.test$Trait, predictedRes$predictions, mode = "everything") #confusion matrix

multiclass.roc(dataTrait.test$Trait, predictedProb$predictions) #auroc value

stopCluster(myCl) #stop parallel computing