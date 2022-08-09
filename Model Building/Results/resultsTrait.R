library(readxl)
library(tidyverse)

resultsTrait <- read_excel("Model Building/Results/results.xlsx", sheet = "Trait", na = "NA")

resultsTrait$Classifier <- factor(resultsTrait$Classifier)
resultsTrait$TrainTestPartition <- factor(resultsTrait$TrainTestPartition)

#accuracy----
resultsTrait %>%
  ggplot(aes(x=TrainTestPartition, y=Accuracy, group=Classifier, color=Classifier)) +
  geom_line() +
  ggtitle("Accuracy of Classifiers for Different Train/Test Dataset Partition\n for Trait Classification")

#auroc----
resultsTrait %>%
  ggplot(aes(x=TrainTestPartition, y=AUROC, group=Classifier, color=Classifier)) +
  geom_line() +
  ggtitle("AUROC of Classifiers for Different Train/Test Dataset Partition\n for Trait Classification")