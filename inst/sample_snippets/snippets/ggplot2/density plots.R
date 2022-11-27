library(ggplot2)

# Create random data
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)),
                  rating = c(rnorm(200),rnorm(200, mean=.8)))

ggplot(dat, aes(x=rating, fill=cond)) + geom_density(alpha=.3)
