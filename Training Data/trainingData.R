#script to combine every features into training data set

library(tidyverse)
library(openxlsx)

rawFeatures <- read.xlsx("rawFeatures.xlsx")

coexpAndPpi <- function(filename){ #check if gene present in coExp and PPI
  a <- filename %>% 
    select(a_OsID) %>%
    rename(OsID = a_OsID)
  
  b <- filename %>% 
    select(b_OsID) %>%
    rename(OsID = b_OsID)
  
  temp <- rbind(a,b) %>% #select and merge gene a and b together
    distinct() #remove duplicates
  
  return(temp)
}

#Co-expression Network----
coExpression <- read.xlsx("Data Preprocessing/coExpression.xlsx")
tempCoExp <- coexpAndPpi(coExpression)
rawFeatures$CoExpression <- as.integer(rawFeatures$OsID %in% tempCoExp$OsID) #check if OsID present in coExpression

aOSID <- coExpression %>%
  distinct() %>%
  select(a_OsID,pcc) %>%
  rename(OsID = a_OsID)

bOSID <- coExpression %>%
  distinct() %>%
  select(b_OsID,pcc) %>%
  rename(OsID = b_OsID)

rawFeatures <- rawFeatures %>%
  left_join(distinct(rbind(aOSID,bOSID)), by="OsID") %>% #add pcc column
  mutate_all(~replace(., is.na(.), 0)) #replace NA with 0

#PPI Network----
ppiNetwork <- read.xlsx("Data Preprocessing/ppiNetwork.xlsx")
tempPPI <- coexpAndPpi(ppiNetwork)
rawFeatures$PPI <- as.integer(rawFeatures$OsID %in% tempPPI$OsID) #check if OsID present in PPI

#ET----
groupByTraits <- read.xlsx("Data Preprocessing/ET/groupByTraits.xlsx", sheet = "All")
traits <- read.xlsx("Data Preprocessing/ET/groupByTraits.xlsx", sheet = "Traits")

groupByTraits <- groupByTraits %>%
  mutate(ET = case_when(
    `Up/Down.regulated`=="Down regulated" ~ 1,
    `Up/Down.regulated`=="Up regulated" ~ 2))

rawFeatures <- groupByTraits %>%
  select(OsID,log_2.fold.change,ET) %>%
  right_join(rawFeatures) %>% #keep all genes from rawFeatures
  mutate_at(vars(log_2.fold.change,ET), ~replace_na(., 0)) %>% #replace NAs from log_2 fold change and ET to 0 (no expression)
  arrange(OsID) %>% #sort OsID in ascending order
  mutate(Class = as.integer(fct_inorder(OsID))) %>% #labeling according to OsID
  left_join(traits) %>% #labeling according to traits
  distinct() #keep only unique values

#EV----
evPattern <- read.xlsx("Data Preprocessing/EV/evPattern.xlsx")

evPatternTest <- evPattern %>%
  split(evPattern$OsID) %>% #split into list of data frames according to OsID
  map(function(x) summarise(x,across(where(is.character), ~OsID),across(where(is.numeric), sum,na.rm = TRUE))) %>% #add new row for sum value of each column
  bind_rows() %>% #combine list of data frames to a single data frame
  distinct() %>%
  mutate_if(is.numeric, ~replace(., . == 0, NA)) #replace zero with NA

OsID <- evPatternTest[,1, drop=FALSE] #extract OsID column

evPattern <- as.data.frame(t(apply(evPatternTest[,2:16], 1, function(x) replace(x, x != max(x, na.rm = TRUE), 0)))) %>% #keep only max value of row and convert other values to 0, ignoring OsID column
  mutate_if(is.numeric, ~1 * (. != 0)) %>% #convert all non-zero numeric values (NA not affected) to 1
  add_column(OsID, .before = 1) #add OsID column to front

rawFeatures <- rawFeatures %>%
  left_join(evPattern) %>%
  distinct(.keep_all = T)

#output results to file----
write.xlsx(rawFeatures,"trainingData.xlsx")

#further formatting is done in Excel, ie changing columns' position