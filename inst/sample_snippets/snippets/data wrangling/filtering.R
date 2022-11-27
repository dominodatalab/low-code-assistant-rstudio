library(dplyr)
library(gapminder)

# Filter rows with the year 1972
gapminder %>%
  select(country, year, lifeExp) %>%
  filter(year == 1972)

# Filter rows with the year 1972 and with a life expectancy below average
gapminder %>%
  select(country, year, lifeExp) %>%
  filter(
    year == 1972,
    lifeExp < mean(lifeExp)
  )

# Filter rows with the year 1972 and with a life expectancy below average only in Africa or Americas
gapminder %>%
  select(country, year, lifeExp, continent) %>%
  filter(
    year == 1972,
    lifeExp < mean(lifeExp),
    continent == "Africa" | continent == "Americas"
  )
