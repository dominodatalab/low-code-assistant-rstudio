# Taken from https://r-graph-gallery.com/

# Load ggplot2
library(ggplot2)

# Create data
data <- data.frame(
  name=c("A","B","C","D","E") ,
  value=c(3,12,5,18,45)
)

ggplot(data, aes(x=name, y=value, fill=name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")
