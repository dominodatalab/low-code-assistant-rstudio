# Taken from https://r-graph-gallery.com/

# library
library(ggplot2)

# dataset:
data=data.frame(value=rnorm(1000, sd = 50))

# basic histogram
ggplot(data, aes(x=value)) +
  geom_histogram(binwidth=10, fill="#69b3a2", color="#e9ecef")
