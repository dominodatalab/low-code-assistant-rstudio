library(dplyr)
library(gapminder)

# Create a column that combines continent and coountry information,
# and another column that shows the rounded lifeExp information
gapminder %>%
  arrange(year, pop) %>%
  mutate(
    con_country = paste(continent, "-", country),
    rn_lifeExp = round(lifeExp)
  ) %>%
  select(continent, country, con_country, lifeExp, rn_lifeExp)

