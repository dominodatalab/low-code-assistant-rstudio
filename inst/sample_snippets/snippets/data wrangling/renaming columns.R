library(dplyr)
library(gapminder)

gapminder %>%
  select(country, year, lifeExp) %>%
  rename(
    Year = year,
    "Life Expectancy" = lifeExp
  )
