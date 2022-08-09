#gene classification (grouping of the same genes together)

library(readxl)
library(tidyverse)
library(caret)
library(Matrix)
library(xgboost)
library(doParallel)
library(pROC)

trainingData <- read_excel("Training Data/trainingData.xlsx")

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

class <- dataClass$Class
label <- as.integer(dataClass$Class)-1
dataClass$Class <- NULL
n <- nrow(dataClass)

#train-test data partition split
train.index <- sample(n,floor(0.70*n))
# accuracy	kappa	auroc
# 0.8719	0.8701	0.9879

train.index <- sample(n,floor(0.75*n))
# accuracy	kappa	auroc
# 0.8716	0.8698	0.9881

train.index <- sample(n,floor(0.80*n))
# accuracy	kappa	auroc
# 0.8776	0.8759	0.9883

train.index <- sample(n,floor(0.85*n))
# accuracy auroc
# 0.8774 0.988

train.index <- sample(n,floor(0.90*n))
# accuracy auroc
# 0.8793 0.988

train.index <- sample(n,floor(0.95*n))
# accuracy auroc
# 0.8794 0.9874

# split train data and make xgb.DMatrix
train.data <- dataClass[train.index,]
train.data <- matrix(as.numeric(unlist(train.data)), ncol = ncol(train.data)) # Convert to numeric matrix
train.label <- label[train.index]

# split test data and make xgb.DMatrix
test.data <- dataClass[-train.index,]
test.data <- matrix(as.numeric(unlist(test.data)), ncol = ncol(test.data))
test.label <- label[-train.index]

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

#parallel computing
myCl <- makeCluster(detectCores())
registerDoParallel(myCl)

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

# Review the final model and results
xgb.fit

# Predict outcomes with the test data
xgb.pred <- predict(xgb.fit,test.data,reshape=T)
xgb.pred <- as.data.frame(xgb.pred)
colnames(xgb.pred) <- levels(class)

# Use the predicted label with the highest probability
xgb.pred$prediction <- apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label <- levels(class)[test.label+1]

# Calculate the final accuracy
result <- sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.4f", result)))

# Confusion matrix and auroc value
confusionMatrix(factor(xgb.pred$prediction), factor(xgb.pred$label), mode = "everything")
multiclass.roc(xgb.pred$label, xgb.pred)

stopCluster(myCl) #stop parallel computing
