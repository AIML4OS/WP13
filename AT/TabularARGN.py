# !pip install -U 'mostlyai[local]'

# initialize the SDK
from mostlyai.sdk import MostlyAI
mostly = MostlyAI(local = True)

import pandas as pd

df_house = pd.read_csv(
  "eusilcP_house.csv"
)
df_pers = pd.read_csv(
  "eusilcP_pers.csv"
)

# train a single-table generator, with default configs
g = mostly.train(
  config = {
    'name': 'EUSILC Pop',
    'tables': [{
        'name': 'households',
        'data': df_house,
        'primary_key': 'hid',
    }, {
        'name': 'people',
        'data': df_pers,
        'foreign_keys': [{
            'column': 'hid',
            'referenced_table': 'households',
            'is_context': True
        }]
    }]
    
  }, start=True, wait=True
)

# display the quality assurance report
g.reports(display=True)

# generate a representative synthetic dataset, with default configs
sd = mostly.generate(g)
df = sd.data()

df['households'].to_csv("eusilcP_house_TabularARGAN.csv")
df['people'].to_csv("eusilcP_pers_TabularARGAN.csv")
