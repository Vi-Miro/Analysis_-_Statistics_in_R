import pandas as pd

data = pd.read_csv('abalone.csv')

df = data.query('Sex == "M" or Sex == "F"')

female = df.query('Sex == "F"').Rings
male = df.query('Sex == "M"').Rings.head(1307)


print(diff)
