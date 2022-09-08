#trait_knn
#train-test split ratio from 0.6 to 0.9
#each ratio run 5 times

library(openxlsx)
library(tidyverse)
library(caret)
library(doParallel)

trainingData <- read.xlsx("trainingData.xlsx")

dataTrait <- trainingData %>%
  select(-Class) %>% #exclude Class column
  mutate_all(~replace(., is.na(.), 2)) #replace NA as 2

col_names <- names(dataTrait[,c(3:4,6:22)]) #exclude OsID (character), log2 fold change and pcc columns (numeric)
dataTrait[col_names] <- lapply(dataTrait[col_names],factor) #convert the rest of column to factor

#parallel computing----
myCl <- makeCluster(detectCores())
registerDoParallel(myCl)

#set seed from 1 to 5
for (i in seq(from = 1, to = 5, by = 1)) {
  set.seed(i)
  
  #train-test split ratio from 0.6 to 0.9
  for (j in seq(from = 0.6, to = 0.9, by = 0.1)) {
    index <- createDataPartition(dataTrait$Trait, p = j, list = FALSE) #train-test data partition split
    
    #split into train and test data
    dataTrait.training <- dataTrait[index,]
    dataTrait.test <- dataTrait[-index,]
    
    x <- select(dataTrait.training, -c(OsID,Trait)) #predictor
    y <- dataTrait.training$Trait #response
    
    fitControl <- trainControl(
      method = "repeatedcv", #cross validation
      number = 10, #10-fold 
      repeats = 5, #repeated 5 times
      allowParallel = T) 
    
    #model training
    model <- train(x,y,
                   method = "knn",
                   trControl = fitControl)
    
    #generate predicted outcome----
    predicted <- predict(model, dataTrait.test) #factor of predicted traits
    predictedProb <- predict(model, dataTrait.test, type = "prob") #probability of predicted traits
    
    #prediction table contains predicted trait for each row
    prediction <- tibble(predicted) %>% 
      add_column(dataTrait.test$OsID, .before = 1) %>%
      rename(OsID = "dataTrait.test$OsID") %>%
      rowid_to_column("ID") #add new ID column
    
    #reference table contains original (from testing data) trait for each row
    reference <- dataTrait.test %>% 
      select(OsID, Trait) %>%
      rename(reference = Trait) %>%
      rowid_to_column("ID") #add new ID column
    
    #comparison table between predicted and original trait for each row
    comparison <- prediction %>%  
      full_join(reference) %>% 
      mutate(comparison = if_else(predicted == reference, TRUE, FALSE)) %>% #true if predicted same as original, else false
      select(-ID)
    
    assign(paste("comparison", j, i, sep = "_"), comparison) #assign to new variable of each iteration of set.seed and createDataPartition for comparison table
    
    #model performance evaluation----
    confusionMatrix <- confusionMatrix(dataTrait.test$Trait, predicted, mode = "everything") #confusion matrix
    
    assign(paste("confusionMatrix", j, i, sep = "_"), confusionMatrix[["table"]]) #assign to new variable of each iteration of set.seed and createDataPartition for confusion matrix
    assign(paste("accuracy", j, i, sep = "_"), confusionMatrix[["overall"]][["Accuracy"]]) #assign to new variable of each iteration of set.seed and createDataPartition for accuracy
  }
}

variables <- mget(ls()) #get all variables in current environment

accuracy <- variables[grep("accuracy", names(variables))] #extract all accuracy data
comparison <- variables[grep("comparison_", names(variables))] #extract all comparison data
confusionMatrix <- variables[grep("confusionMatrix_", names(variables))] #extract all confusion matrix data

write.xlsx(accuracy, file = "Trait_knn_accuracy.xlsx")
write.xlsx(comparison, file = "Trait_knn_comparison.xlsx")
write.xlsx(confusionMatrix, file = "Trait_knn_confusionMatrix.xlsx")

#stop parallel computing----
stopCluster(myCl) 