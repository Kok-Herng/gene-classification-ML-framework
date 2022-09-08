#gene_xgboost
#train-test split ratio from 0.6 to 0.9
#each ratio run 5 times

library(openxlsx)
library(tidyverse)
library(caret)
library(Matrix)
library(xgboost)
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

class <- dataClass$Class
label <- as.integer(dataClass$Class)-1
dataClass$Class <- NULL
n <- nrow(dataClass)

#set seed from 1 to 5
for (i in seq(from = 1, to = 5, by = 1)) {
  set.seed(i)
  
  tryCatch({ #to ignore the error and continue the loop
    #train-test split ratio from 0.6 to 0.9
    for (j in seq(from = 0.6, to = 0.9, by = 0.1)) {
      train.index <- sample(n,floor(j*n))
      
      # split train data and make xgb.DMatrix
      train.data <- dataClass[train.index,]
      train.data <- matrix(as.numeric(unlist(train.data[, -1])), ncol = ncol(train.data[, -1])) # Convert to numeric matrix (ignoring OsID column)
      train.label <- label[train.index]
      
      # split test data and make xgb.DMatrix
      test.data <- dataClass[-train.index,]
      test.data <- matrix(as.numeric(unlist(test.data[, -1])), ncol = ncol(test.data[, -1])) # Convert to numeric matrix (ignoring OsID column)
      test.label <- label[-train.index]
      dataClass.test <- dataClass[-train.index,] #new variable for comparison later
      
      # Transform the two data sets into xgb.Matrix
      xgb.train <- xgb.DMatrix(data=train.data,label=train.label)
      xgb.test <- xgb.DMatrix(data=test.data,label=test.label)
      
      # Define the parameters for multinomial classification
      num_class <- length(levels(class))
      params <- list(
        booster="gbtree",
        eta=0.3,
        max_depth=10,
        gamma=2,
        subsample=1,
        colsample_bytree=1,
        max_delta_step=1,
        objective="multi:softprob",
        num_class=num_class
      )
      
      # Train the XGBoost classifer
      xgb.fit <- xgb.train(
        params=params,
        data=xgb.train,
        nrounds=25,
        nthreads=1,
        early_stopping_rounds=10,
        watchlist=list(val1=xgb.train,val2=xgb.test),
        verbose=1
      )
      
      # Predict outcomes with the test data
      xgb.pred <- predict(xgb.fit,test.data,reshape=T)
      xgb.pred <- as.data.frame(xgb.pred)
      colnames(xgb.pred) <- levels(class)
      
      # Use the predicted label with the highest probability
      xgb.pred$prediction <- apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
      xgb.pred$label <- levels(class)[test.label+1]
      
      #prediction table contains predicted class for each row
      prediction <- tibble(factor(xgb.pred$prediction)) %>% 
        add_column(dataClass.test$OsID, .before = 1) %>%
        rename(OsID = "dataClass.test$OsID",
               predicted = "factor(xgb.pred$prediction)") %>%
        rowid_to_column("ID") #add new ID column
      
      #reference table contains original (from testing data) class for each row
      reference <- tibble(factor(xgb.pred$label)) %>% 
        add_column(dataClass.test$OsID, .before = 1) %>%
        rename(OsID = "dataClass.test$OsID",
               reference = "factor(xgb.pred$label)") %>%
        rowid_to_column("ID") #add new ID column
      
      #comparison table between predicted and original class for each row
      comparison <- prediction %>%  
        full_join(reference) %>% 
        mutate(comparison = if_else(as.character(predicted) == as.character(reference), TRUE, FALSE)) %>% #true if predicted same as original, else false
        select(-ID)
      
      assign(paste("comparison", j, i, sep = "_"), comparison) #assign to new variable of each iteration of set.seed and createDataPartition for comparison table
      
      #model performance evaluation----
      confusionMatrix <- confusionMatrix(factor(xgb.pred$prediction), factor(xgb.pred$label), mode = "everything") #confusion matrix
      
      assign(paste("confusionMatrix", j, i, sep = "_"), confusionMatrix[["table"]]) #assign to new variable of each iteration of set.seed and createDataPartition for confusion matrix
      assign(paste("accuracy", j, i, sep = "_"), confusionMatrix[["overall"]][["Accuracy"]]) #assign to new variable of each iteration of set.seed and createDataPartition for accuracy
    }
  }, error=function(e){}) #error handling function that does nothing
}

variables <- mget(ls()) #get all variables in current environment

accuracy <- variables[grep("accuracy", names(variables))] #extract all accuracy data
comparison <- variables[grep("comparison_", names(variables))] #extract all comparison data
confusionMatrix <- variables[grep("confusionMatrix_", names(variables))] #extract all confusion matrix data

write.xlsx(accuracy, file = "Gene_xgBoost_accuracy.xlsx")
write.xlsx(comparison, file = "Gene_xgBoost_comparison.xlsx")
write.xlsx(confusionMatrix, file = "Gene_xgBoost_confusionMatrix.xlsx")

#stop parallel computing----
stopCluster(myCl) 