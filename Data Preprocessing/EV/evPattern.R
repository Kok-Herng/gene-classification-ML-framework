#script to group genes and their expression value according to tissue and development stage

library(readr)
library(tidyverse)
library(openxlsx)

outEV2 <- read_csv("outEV2.csv")

evPatternTemp <- outEV2 %>% 
  select(!LocID) %>%
  filter(Expression.value > 2.5) %>%
  rowid_to_column("ID") %>% #add new column named "ID"
  pivot_wider(id_cols = ID, #group expression value according to ID (rows)
              names_from = c(Tissue, Development.stage), #tissue and development stage (columns)
              values_from = Expression.value)

evPatternTemp$ID <- outEV2 %>% #replace values in ID column with respective OsID
  filter(Expression.value > 2.5) %>%
  pull(OsID) #extract a single column

evPattern <- evPatternTemp %>%
  rename(OsID = ID) %>%
  group_split(OsID) %>% #split into list of lists according to OsID
  lapply(function(y) lapply(y, function(x) { #move non-NA values to in front of each column
    c(x[!is.na(x)], x[is.na(x)])
    })) %>%
  lapply(bind_rows) %>% #convert individual list to dataframe, making a list of dataframes
  bind_rows() %>% #convert the list of dataframes to a single dataframe
  filter_at(-1, any_vars(!is.na(.))) %>% #remove rows with all NA except for OsID column
  write.xlsx("evPattern.xlsx")

#further formatting is done in Excel, ie rename column names and merge tissue
