#trait_xgBoost
#train-test split ratio from 0.6 to 0.9
#each ratio run 5 times

library(openxlsx)
library(tidyverse)
library(caret)
library(Matrix)
library(xgboost)
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

trait <- dataTrait$Trait
label <- as.integer(dataTrait$Trait)-1
dataTrait$Trait <- NULL
n <- nrow(dataTrait)

#set seed from 1 to 5
for (i in seq(from = 1, to = 5, by = 1)) {
  set.seed(i)
  
  #train-test split ratio from 0.6 to 0.9
  for (j in seq(from = 0.6, to = 0.9, by = 0.1)) {
    train.index <- sample(n,floor(j*n))
    
    # split train data and make xgb.DMatrix
    train.data <- dataTrait[train.index,]
    train.data <- matrix(as.numeric(unlist(train.data[, -1])), ncol = ncol(train.data[, -1])) # Convert to numeric matrix (ignoring OsID column)
    train.label <- label[train.index]
    
    # split test data and make xgb.DMatrix
    test.data <- dataTrait[-train.index,]
    test.data <- matrix(as.numeric(unlist(test.data[, -1])), ncol = ncol(test.data[, -1])) # Convert to numeric matrix (ignoring OsID column)
    test.label <- label[-train.index]
    dataTrait.test <- dataTrait[-train.index,] #new variable for comparison later
    
    # Transform the two data sets into xgb.Matrix
    xgb.train <- xgb.DMatrix(data=train.data,label=train.label)
    xgb.test <- xgb.DMatrix(data=test.data,label=test.label)
    
    # Define the parameters for multinomial classification
    num_class <- length(levels(trait))
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
    colnames(xgb.pred) <- levels(trait)
    
    # Use the predicted label with the highest probability
    xgb.pred$prediction <- apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
    xgb.pred$label <- levels(trait)[test.label+1]
    
    #prediction table contains predicted trait for each row
    prediction <- tibble(factor(xgb.pred$prediction)) %>% 
      add_column(dataTrait.test$OsID, .before = 1) %>%
      rename(OsID = "dataTrait.test$OsID",
             predicted = "factor(xgb.pred$prediction)") %>%
      rowid_to_column("ID") #add new ID column
    
    #reference table contains original (from testing data) trait for each row
    reference <- tibble(factor(xgb.pred$label)) %>% 
      add_column(dataTrait.test$OsID, .before = 1) %>%
      rename(OsID = "dataTrait.test$OsID",
             reference = "factor(xgb.pred$label)") %>%
      rowid_to_column("ID") #add new ID column
    
    #comparison table between predicted and original trait for each row
    comparison <- prediction %>%  
      full_join(reference) %>% 
      mutate(comparison = if_else(predicted == reference, TRUE, FALSE)) %>% #true if predicted same as original, else false
      select(-ID)
    
    assign(paste("comparison", j, i, sep = "_"), comparison) #assign to new variable of each iteration of set.seed and createDataPartition for comparison table
    
    #model performance evaluation----
    confusionMatrix <- confusionMatrix(factor(xgb.pred$prediction), factor(xgb.pred$label), mode = "everything") #confusion matrix
    
    assign(paste("confusionMatrix", j, i, sep = "_"), confusionMatrix[["table"]]) #assign to new variable of each iteration of set.seed and createDataPartition for confusion matrix
    assign(paste("accuracy", j, i, sep = "_"), confusionMatrix[["overall"]][["Accuracy"]]) #assign to new variable of each iteration of set.seed and createDataPartition for accuracy
  }
}

variables <- mget(ls()) #get all variables in current environment

accuracy <- variables[grep("accuracy", names(variables))] #extract all accuracy data
comparison <- variables[grep("comparison_", names(variables))] #extract all comparison data
confusionMatrix <- variables[grep("confusionMatrix_", names(variables))] #extract all confusion matrix data

write.xlsx(accuracy, file = "Trait_xgBoost_accuracy.xlsx")
write.xlsx(comparison, file = "Trait_xgBoost_comparison.xlsx")
write.xlsx(confusionMatrix, file = "Trait_xgBoost_confusionMatrix.xlsx")

#stop parallel computing----
stopCluster(myCl) 