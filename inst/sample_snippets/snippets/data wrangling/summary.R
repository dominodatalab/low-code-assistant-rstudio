library(dplyr)
library(gapminder)

# Calculate mean and standard deviation for population and life expectations
gapminder %>%
  summarize(
    pop_mean = mean(pop),
    pop_sd = sd(pop),
    le_mean = mean(lifeExp),
    le_sd = sd(lifeExp)
  )
