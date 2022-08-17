#script to group genes and their expression value according to tissue and development stage

library(tidyverse)
library(openxlsx)

selectedProj <- read.xlsx("Data Extraction/EV/fltr_sra_fpkm.xlsx", sheet = "fltr_sra_fpkm")
rapdb_msu <- read_delim("rapdb_msu.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

evPatternTemp <- selectedProj %>%
  select(LOC.Gene.ID, Development.stage, Tissue, Expression.value) %>%
  filter(Expression.value > 2.5) %>%
  rowid_to_column("ID") %>% #add new column named "ID"
  pivot_wider(id_cols = ID, #group expression value according to ID (rows)
              names_from = c(Tissue, Development.stage), #tissue and development stage (columns)
              values_from = Expression.value)

evPatternTemp$ID <- selectedProj %>% #replace values in ID column with respective LOC.Gene.ID
  filter(Expression.value > 2.5) %>%
  pull(LOC.Gene.ID) #extract a single column

evPattern <- evPatternTemp %>%
  rename(LOC.Gene.ID = ID) %>%
  group_split(LOC.Gene.ID) %>% #split into list of lists according to LOC.Gene.ID
  lapply(function(y) lapply(y, function(x) { #move non-NA values to in front of each column
    c(x[!is.na(x)], x[is.na(x)])
    })) %>%
  lapply(bind_rows) %>% #convert individual list to dataframe, making a list of dataframes
  bind_rows() %>% #convert the list of dataframes to a single dataframe
  filter_at(-1, any_vars(!is.na(.))) %>% #remove rows with all NA except for LOC.Gene.ID column
  # Replace "LOC.Gene.ID" as "OsID" 
  left_join(rapdb_msu, by = c("LOC.Gene.ID"="LocID")) %>% 
  select(-LOC.Gene.ID) %>%
  select(OsID, everything()) #move "OsID" column to the front 

write.xlsx(evPattern, "evPattern.xlsx") #write output to file

#further formatting is done in Excel, ie rename column names and merge tissue