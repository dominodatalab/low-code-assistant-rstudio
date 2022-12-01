library(tm)

# use the text of The Art of War
file <- system.file("sample_data", "artofwar.txt", package = "assistDomino")
artofwar <- readLines(file)

# find the most common words used in the book
corpus <- Corpus(VectorSource(artofwar))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
tdm <- as.matrix(TermDocumentMatrix(corpus))
data <- sort(rowSums(tdm), decreasing = TRUE)
data <- data.frame(word = names(data), freq = as.numeric(data))
head(data)
