import pandas as pd
import glob
import os

file_path = os.getcwd()

data = []
for csvfile in glob.glob(os.path.join(file_path, "*.csv")):
    df = pd.read_csv(csvfile, encoding="utf-8", delimiter=",")
    data.append(df)

data = pd.concat(data, ignore_index=True)
data.to_csv("outCoExpression.csv")