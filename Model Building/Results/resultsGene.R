library(readxl)
library(tidyverse)

resultsGene <- read_excel("Model Building/Results/results.xlsx", sheet = "Gene", na = "NA")

resultsGene$Classifier <- factor(resultsGene$Classifier)
resultsGene$TrainTestPartition <- factor(resultsGene$TrainTestPartition)

#accuracy----
resultsGene %>%
  ggplot(aes(x=TrainTestPartition, y=Accuracy, group=Classifier, color=Classifier)) +
  geom_line() +
  ggtitle("Accuracy of All Classifiers for Different Train/Test Dataset Partition \nfor Gene Classification")

#auroc for all----
resultsGene %>%
  ggplot(aes(x=TrainTestPartition, y=AUROC, group=Classifier, color=Classifier)) +
  geom_line() +
  ggtitle("AUROC of All Classifiers for Different Train/Test Dataset Partition \nfor Gene Classification")

#auroc for top 3----
resultsGene %>%
  filter(Classifier != "knn" & Classifier != "c5.0") %>%
  ggplot(aes(x=TrainTestPartition, y=AUROC, group=Classifier, color=Classifier)) +
  geom_line() +
  ggtitle("AUROC of Top 3 Classifiers for Different Train/Test Dataset Partition \nfor Gene Classification")