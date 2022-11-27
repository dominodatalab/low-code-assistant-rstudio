# Taken from https://r-graph-gallery.com/

# load ggplot2
library(ggplot2)

# mtcars dataset is natively available in R
# head(mtcars)

# A basic scatterplot with color depending on Species
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=TRUE) +
  theme_minimal()
