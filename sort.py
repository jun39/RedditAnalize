import pandas as pd 

data = pd.read_csv('total2020size30.csv')

data.sort_values(by = 'Publish Date', ascending = True, inplace = True) 

data.to_csv("Sorttotal2020size30.csv",index=False)
