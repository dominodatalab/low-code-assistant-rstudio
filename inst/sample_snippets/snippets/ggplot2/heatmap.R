# Taken from https://r-graph-gallery.com/

# The mtcars dataset:
data <- as.matrix(mtcars)

heatmap(data, Colv = NA, Rowv = NA, scale="column")
