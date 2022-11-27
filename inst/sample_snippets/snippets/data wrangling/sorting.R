library(dplyr)
library(gapminder)

# Sort by year
gapminder %>%
  select(continent, year, lifeExp) %>%
  arrange(year)

# Sort by lifeExp and then by year
gapminder %>%
  select(continent, year, lifeExp) %>%
  arrange(lifeExp, desc(year))
