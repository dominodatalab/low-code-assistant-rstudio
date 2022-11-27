# Taken from https://r-graph-gallery.com/

# The mtcars dataset:
data <- as.matrix(mtcars)

heatmap(data, scale="column", col = terrain.colors(256))
