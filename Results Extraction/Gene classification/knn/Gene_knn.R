#gene_knn
#train-test split ratio from 0.6 to 0.9
#each ratio run 5 times

library(openxlsx)
library(tidyverse)
library(caret)
library(doParallel)

trainingData <- read.xlsx("trainingData.xlsx")

dataClass <- trainingData %>%
  group_by(OsID) %>%
  mutate(Count=n()) %>% #count number of occurrence for each OsID
  ungroup() %>%
  filter(Count>=10) %>% #use only OsID with 10 or more occurrences
  select(-c(Class, Count, Trait)) %>% #exclude Class, Count and Trait columns
  mutate(Class = as.integer(fct_inorder(OsID))) %>% #group same OsID together and assign them a class
  mutate_all(~replace(., is.na(.), 2)) #replace NA as 2 for EV columns

col_names <- names(dataClass[,c(3:4,6:22)]) #exclude OsID (character), log2 fold change and pcc columns (numeric)
dataClass[col_names] <- lapply(dataClass[col_names],factor) #convert the rest of column to factor

#parallel computing----
myCl <- makeCluster(detectCores())
registerDoParallel(myCl)

#set seed from 1 to 5
for (i in seq(from = 1, to = 5, by = 1)) {
  set.seed(i)
  
  #train-test split ratio from 0.6 to 0.9
  for (j in seq(from = 0.6, to = 0.9, by = 0.1)) {
    index <- createDataPartition(dataClass$Class, p = j, list = FALSE) #train-test data partition split
    
    #split into train and test data
    dataClass.training <- dataClass[index,]
    dataClass.test <- dataClass[-index,]
    
    x <- select(dataClass.training, -c(OsID, Class)) #predictor
    y <- dataClass.training$Class #response
    
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
    predicted <- predict(model, dataClass.test) #factor of predicted classes
    predictedProb <- predict(model, dataClass.test, type = "prob") #probability of predicted classes
    
    #prediction table contains predicted class for each row
    prediction <- tibble(predicted) %>% 
      add_column(dataClass.test$OsID, .before = 1) %>%
      rename(OsID = "dataClass.test$OsID") %>%
      rowid_to_column("ID") #add new ID column
    
    #reference table contains original (from testing data) class for each row
    reference <- dataClass.test %>% 
      select(OsID, Class) %>%
      rename(reference = Class) %>%
      rowid_to_column("ID") #add new ID column
    
    #comparison table between predicted and original class for each row
    comparison <- prediction %>%  
      full_join(reference) %>% 
      mutate(comparison = if_else(predicted == reference, TRUE, FALSE)) %>% #true if predicted same as original, else false
      select(-ID)
    
    assign(paste("comparison", j, i, sep = "_"), comparison) #assign to new variable of each iteration of set.seed and createDataPartition for comparison table
    
    #model performance evaluation----
    confusionMatrix <- confusionMatrix(dataClass.test$Class, predicted, mode = "everything") #confusion matrix
    
    assign(paste("confusionMatrix", j, i, sep = "_"), confusionMatrix[["table"]]) #assign to new variable of each iteration of set.seed and createDataPartition for confusion matrix
    assign(paste("accuracy", j, i, sep = "_"), confusionMatrix[["overall"]][["Accuracy"]]) #assign to new variable of each iteration of set.seed and createDataPartition for accuracy
  }
}

variables <- mget(ls()) #get all variables in current environment

accuracy <- variables[grep("accuracy", names(variables))] #extract all accuracy data
comparison <- variables[grep("comparison_", names(variables))] #extract all comparison data
confusionMatrix <- variables[grep("confusionMatrix_", names(variables))] #extract all confusion matrix data

write.xlsx(accuracy, file = "Gene_knn_accuracy.xlsx")
write.xlsx(comparison, file = "Gene_knn_comparison.xlsx")
write.xlsx(confusionMatrix, file = "Gene_knn_confusionMatrix.xlsx")

#stop parallel computing----
stopCluster(myCl)