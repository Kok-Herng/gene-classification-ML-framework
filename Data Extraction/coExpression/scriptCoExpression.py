#script to extract and merge co-expression data from multiple csv files into single file

import pandas as pd
import glob
import os

file_path = os.getcwd() #get current working directory 

data = [] #empty list to store files
for csvfile in glob.glob(os.path.join(file_path, "*.csv")): #find files end with csv
    df = pd.read_csv(csvfile, encoding="utf-8", delimiter=",") #read csv files as dataframe
    data.append(df) #append to list for each csv file

data = pd.concat(data, ignore_index=True) #concatenate csv files and display continuous index value
data.to_csv("outCoExpression2.csv") #output results to new csv file
