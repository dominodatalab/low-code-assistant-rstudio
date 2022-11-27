library(dplyr)
library(gapminder)

# Summarize population and lifeExp grouped by continent
gapminder %>%
  group_by(continent) %>%
  summarize(
    pop_mean = mean(pop),
    pop_sd = sd(pop),
    le_mean = mean(lifeExp),
    le_sd = sd(lifeExp)
  )
