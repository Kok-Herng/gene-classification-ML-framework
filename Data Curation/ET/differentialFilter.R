#script to filter unrelated records from each trait, 
#add new up/down regulated column, and
#merge into single file

library(readr)
library(tidyverse)
library(openxlsx)

coldDifferential <- read_delim("Data Extraction/ET/ebiColdDifferential.tsv", "\t", 
                               escape_double = FALSE, 
                               col_types = cols(`t-statistic` = col_double()), 
                               trim_ws = TRUE)
droughtDifferential <- read_delim("Data Extraction/ET/ebiDroughtDifferential.tsv", "\t", 
                                  escape_double = FALSE, 
                                  col_types = cols(`t-statistic` = col_double()), 
                                  trim_ws = TRUE)
heatDifferential <- read_delim("Data Extraction/ET/ebiHeatDifferential.tsv", "\t", 
                               escape_double = FALSE, 
                               col_types = cols(`t-statistic` = col_double()), 
                               trim_ws = TRUE)
saltDifferential <- read_delim("Data Extraction/ET/ebiSaltDifferential.tsv", "\t", 
                               escape_double = FALSE, 
                               col_types = cols(`t-statistic` = col_double()), 
                               trim_ws = TRUE)
stressDifferential <- read_delim("Data Extraction/ET/ebiStressDifferential.tsv", "\t", 
                                 escape_double = FALSE, 
                                 col_types = cols(`t-statistic` = col_double()), 
                                 trim_ws = TRUE)
submergencyDifferential <- read_delim("Data Extraction/ET/ebiSubmergencyDifferential.tsv", "\t", 
                                      escape_double = FALSE, col_types = cols(`t-statistic` = col_double()), 
                                      trim_ws = TRUE)

coldFiltered <- coldDifferential %>% 
  filter(str_detect(Comparison, 'cold | chill | celsius')) %>% #filter only related conditions
  mutate('Up/Down regulated' = case_when(
    `log_2 fold change` > 0 ~ 'Up regulated',
    `log_2 fold change` < 0 ~ 'Down regulated'
  ))

droughtFiltered <- droughtDifferential %>% 
  filter(str_detect(Comparison, 'drought | dry | PEG')) %>% #filter only related conditions
  mutate('Up/Down regulated' = case_when(
    `log_2 fold change` > 0 ~ 'Up regulated',
    `log_2 fold change` < 0 ~ 'Down regulated'
  ))

heatFiltered <- heatDifferential %>% #no filtering, use all records
  mutate('Up/Down regulated' = case_when(
    `log_2 fold change` > 0 ~ 'Up regulated',
    `log_2 fold change` < 0 ~ 'Down regulated'
  ))

saltFiltered <- saltDifferential %>% 
  filter(str_detect(Comparison, 'cadmium | salt | alkali | sodium')) %>% #filter only related conditions
  mutate('Up/Down regulated' = case_when(
    `log_2 fold change` > 0 ~ 'Up regulated',
    `log_2 fold change` < 0 ~ 'Down regulated'
  ))

stressFiltered <- stressDifferential %>% #no filtering, use all records
  mutate('Up/Down regulated' = case_when(
    `log_2 fold change` > 0 ~ 'Up regulated',
    `log_2 fold change` < 0 ~ 'Down regulated'
  ))

submergencyFiltered <- submergencyDifferential %>% 
  filter(str_detect(Comparison, 'anaerobic | submergence | oxygen')) %>% #filter only related conditions
  mutate('Up/Down regulated' = case_when(
    `log_2 fold change` > 0 ~ 'Up regulated',
    `log_2 fold change` < 0 ~ 'Down regulated'
  ))

sheetNames <- list('Cold' = coldFiltered, 'Drought' = droughtFiltered, 'Heat' = heatFiltered, 'Salt' = saltFiltered,
                   'Stress' = stressFiltered, 'Submergency' = submergencyFiltered)
write.xlsx(sheetNames, file = 'differentialFiltered.xlsx') #write output to file

#extra steps done in Excel to further exclude out unrelated rows like
#other stresses and disease traits