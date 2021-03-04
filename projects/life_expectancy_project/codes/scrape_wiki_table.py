# This code scrapes the content of the wikipedia page on life expectancy by country and saves the statistics in a csv file
import pandas as pd
link = "https://en.wikipedia.org/wiki/List_of_countries_by_life_expectancy"

# Get the table
table = pd.read_html(link,header=0)[4]

# Rename the columns of the dataframe
table.columns = ["country", "male", "female", "both", "adjusted"]

# Save the csv file
table.to_csv('./data/life_exp.csv', sep=',', index=False)
