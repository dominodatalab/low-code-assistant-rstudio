library(dplyr)
library(gapminder)

#  Selecting Columns in your Data Set
gapminder %>%
  select(continent, year, pop)

# Selecting all columns but the year column
gapminder %>%
  select(-year)

# Selecting all columns that start with co
gapminder %>%
  select(starts_with("co"))

