#trait classification (classification of related traits for each gene)

library(readxl)
library(tidyverse)
library(caret)
library(Matrix)
library(xgboost)
library(doParallel)
library(pROC)

trainingData <- read_excel("Training Data/trainingData.xlsx")

dataTrait <- trainingData %>%
  select(-c(OsID,Class)) %>% #exclude OsID and Class column
  mutate_all(~replace(., is.na(.), 2)) #replace NA as 2

col_names <- names(dataTrait[,c(2:3,5:21)]) #exclude log2 fold change and pcc columns (numeric)
dataTrait[col_names] <- lapply(dataTrait[col_names],factor) #convert the rest of column to factor

set.seed(1234)

trait <- dataTrait$Trait
label <- as.integer(dataTrait$Trait)-1
dataTrait$Trait <- NULL
n <- nrow(dataTrait)

#train-test data partition split
train.index <- sample(n,floor(0.70*n))
# accuracy	kappa	auroc
# 0.5429	0.4066	0.9148

train.index <- sample(n,floor(0.75*n))
# accuracy	kappa	auroc
# 0.5382	0.4021	0.9151

train.index <- sample(n,floor(0.80*n))
# accuracy	kappa	auroc
# 0.5323	0.3929	0.9166

train.index <- sample(n,floor(0.85*n))
# accuracy	kappa	auroc
# 0.5346	0.3975	0.9184

train.index <- sample(n,floor(0.90*n))
# accuracy	kappa	auroc
# 0.523	0.3851	0.9173

train.index <- sample(n,floor(0.95*n))
# accuracy	kappa	auroc
# 0.54	0.4054	0.92

# split train data and make xgb.DMatrix
train.data <- dataTrait[train.index,]
train.data <- matrix(as.numeric(unlist(train.data)), ncol = ncol(train.data)) # Convert to numeric matrix
train.label <- label[train.index]

# split test data and make xgb.DMatrix
test.data <- dataTrait[-train.index,]
test.data <- matrix(as.numeric(unlist(test.data)), ncol = ncol(test.data)) # Convert to numeric matrix
test.label <- label[-train.index]

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
colnames(xgb.pred) <- levels(trait)

# Use the predicted label with the highest probability
xgb.pred$prediction <- apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label <- levels(trait)[test.label+1]

# Confusion matrix and auroc value
confusionMatrix(factor(xgb.pred$prediction), factor(xgb.pred$label), mode = "everything")
multiclass.roc(xgb.pred$label, xgb.pred)

stopCluster(myCl) #stop parallel computing