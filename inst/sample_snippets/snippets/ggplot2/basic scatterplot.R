# Taken from https://r-graph-gallery.com/

# library
library(ggplot2)

# The iris dataset is provided natively by R
#head(iris)

# basic scatterplot
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point()
