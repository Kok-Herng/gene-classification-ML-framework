#script to group genes and their expression value according to each trait

library(readxl)
library(readr)
library(writexl)

#each file contains genes related to each trait
cold <- read_excel("cold.xlsx", col_names = FALSE)
drought <- read_excel("drought.xlsx", col_names = FALSE)
heat <- read_excel("heat.xlsx", col_names = FALSE)
salt <- read_excel("salt.xlsx", col_names = FALSE)
stressResponse <- read_excel("stress response.xlsx", col_names = FALSE)
submergency <- read_excel("submergency.xlsx", col_names = FALSE)

#this file contains all the genes and their expression value
outEV2 <- read_csv("outEV2.csv")

colnames(cold) <- c('OsID','Traits')
colnames(drought) <- c('OsID','Traits')
colnames(heat) <- c('OsID','Traits')
colnames(salt) <- c('OsID','Traits')
colnames(stressResponse) <- c('OsID','Traits')
colnames(submergency) <- c('OsID','Traits')

#only keep rows that match from both of the data frames
dfCold = merge(x=outEV2,y=cold,by='OsID')
dfDrought = merge(x=outEV2,y=drought,by='OsID')
dfHeat = merge(x=outEV2,y=heat,by='OsID')
dfSalt = merge(x=outEV2,y=salt,by='OsID')
dfStressResponse = merge(x=outEV2,y=stressResponse,by='OsID')
dfSubmergency = merge(x=outEV2,y=submergency,by='OsID')

write_xlsx(dfCold,'coldMerged.xlsx')
write_xlsx(dfDrought,'droughtMerged.xlsx')
write_xlsx(dfHeat,'heatMerged.xlsx')
write_xlsx(dfSalt,'saltMerged.xlsx')
write_xlsx(dfStressResponse,'stressResponseMerged.xlsx')
write_xlsx(dfSubmergency,'submergencyMerged.xlsx')
