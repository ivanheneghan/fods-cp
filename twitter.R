# Install packages
install.packages("base64enc")
install.packages("twitteR")
install.packages("RCurl")
install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("fpc")

# Load relevant libraries
library(twitteR)
library(RCurl)
library(stringr)
library(tm)
library(dplyr)
library(plyr)
library(reshape2)
library(tidyr)
library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(fpc)   
library(cluster)
library(ggplot2)

# Set up Twitter API credentials
consumer_key <- "jnUHsr3bg5QscbQcIMF9xei1A"
consumer_secret <- "6GJQGIUA63kfO8uIKz8I6fQI7BA9KTFPXDofp6HxvxEJF8Wu4d"
access_token <- "14611173-xXtSpD44S8A1zWv4Qzit6DwdXOuIC4hm57WvZycHA"
access_secret <- "AMxapPQJZnadlTqns1JyoUBquVNfov6AqEt9Pn1z3jpFI"

# Create Twitter connection
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Get Twitter data for each of the 4 main hashtags - 10k tweets for each, from 6/2 onwards, English language tweets only
superbowl <- searchTwitter("#superbowl", n = 10000, lang = "en", since = "2015-02-06")
superbowl50 <- searchTwitter("#superbowl50", n = 10000, lang = "en", since = "2015-02-06")
nfl <- searchTwitter("#nfl", n = 10000, lang = "en", since = "2015-02-06")
sb50 <- searchTwitter("#sb50", n = 10000, lang = "en", since = "2015-02-06")

# Strip retweets
superbowl_no_rt = strip_retweets(superbowl)
superbowl50_no_rt = strip_retweets(superbowl50)
nfl_no_rt = strip_retweets(nfl)
sb50_no_rt = strip_retweets(sb50)

# Create data frames for use later
superbowl_df <- do.call("rbind", lapply(superbowl_no_rt, as.data.frame))
superbowl50_df <- do.call("rbind", lapply(superbowl50_no_rt, as.data.frame))
nfl_df <- do.call("rbind", lapply(nfl_no_rt, as.data.frame))
sb50_df <- do.call("rbind", lapply(sb50_no_rt, as.data.frame))

combined_df <- rbind(superbowl_df, superbowl50_df)
combined_df <- rbind(combined_df, nfl_df)
combined_df <- rbind(combined_df, sb50_df)

# 1. Text Analysis Pre-Processing

# Extract only the text, for manipulation purposes
superbowl_text <- sapply(superbowl_no_rt, function(x) x$getText())
superbowl50_text <- sapply(superbowl50_no_rt, function(x) x$getText())
nfl_text <- sapply(nfl_no_rt, function(x) x$getText())
sb50_text <- sapply(sb50_no_rt, function(x) x$getText())

# Remove RT text
superbowl_text <- gsub("rt", "", superbowl_text)
superbowl50_text <- gsub("rt", "", superbowl50_text)
nfl_text <- gsub("rt", "", nfl_text)
sb50_text <- gsub("rt", "", sb50_text)

# Remove mentions
superbowl_text <- gsub("@\\w+", "", superbowl_text)
superbowl50_text <- gsub("@\\w+", "", superbowl50_text)
nfl_text <- gsub("@\\w+", "", nfl_text)
sb50_text <- gsub("@\\w+", "", sb50_text)

# Remove any punctuation
superbowl_text <- gsub("[[:punct:]]", "", superbowl_text)
superbowl50_text <- gsub("[[:punct:]]", "", superbowl50_text)
nfl_text <- gsub("[[:punct:]]", "", nfl_text)
sb50_text <- gsub("[[:punct:]]", "", sb50_text)

# Remove links
superbowl_text <- gsub("http\\w+", "", superbowl_text)
superbowl50_text <- gsub("http\\w+", "", superbowl50_text)
nfl_text <- gsub("http\\w+", "", nfl_text)
sb50_text <- gsub("http\\w+", "", sb50_text)

# Remove unnecessary characters
superbowl_text <- gsub("[ |\t]{2,}", "", superbowl_text)
superbowl50_text <- gsub("[ |\t]{2,}", "", superbowl50_text)
nfl_text <- gsub("[ |\t]{2,}", "", nfl_text)
sb50_text <- gsub("[ |\t]{2,}", "", sb50_text)

superbowl_text <- gsub("^ ", "", superbowl_text)
superbowl50_text <- gsub("^ ", "", superbowl50_text)
nfl_text <- gsub("^ ", "", nfl_text)
sb50_text <- gsub("^ ", "", sb50_text)

superbowl_text <- gsub(" $", "", superbowl_text)
superbowl50_text <- gsub(" $", "", superbowl50_text)
nfl_text <- gsub(" $", "", nfl_text)
sb50_text <- gsub(" $", "", sb50_text)

# Convert the texts to Corpi
superbowl_text_corpus <- Corpus(VectorSource(superbowl_text))
superbowl50_text_corpus <- Corpus(VectorSource(superbowl50_text))
nfl_text_corpus <- Corpus(VectorSource(nfl_text))
sb50_text_corpus <- Corpus(VectorSource(sb50_text))

# Comine all Corpi
combined_text_corpus <- c(superbowl_text_corpus, superbowl50_text_corpus, nfl_text_corpus, sb50_text_corpus)

# Corpi transformations
# Convert all text to lowercase
combined_text_corpus <- tm_map(combined_text_corpus, content_transformer(tolower))
# Remove any stopwords
combined_text_corpus <- tm_map(combined_text_corpus, removeWords, stopwords("english"))
# Stem the text
combined_text_corpus <- tm_map(combined_text_corpus, stemDocument)
# Remove any whitespace
combined_text_corpus <- tm_map(combined_text_corpus, stripWhitespace)

# Create DTM & Sparse DTM
combined_DTM <- DocumentTermMatrix(combined_text_corpus)
combined_DTMs <- removeSparseTerms(combined_DTM, 0.985)

# 2. Initial investigations - frequent terms & associations for a number of those frequent words, and associated viz

# Frequency

## Frequency variables
combined_freq <- colSums(as.matrix(combined_DTMs))
word_freq <- data.frame(word = names(combined_freq), freq = combined_freq)
min_freq <- 250

## Frequency plot of words, using data frame
ggplot(data = subset(word_freq, combined_freq > min_freq), aes(word, freq)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Wordcloud, using corpus
wordcloud(combined_text_corpus, min.freq = min_freq, scale = c(4, 0.5), colors = brewer.pal(8, "Dark2"),  random.color = TRUE, random.order = FALSE, max.words = 250)

# Associations

## Look at the top words
findFreqTerms(combined_DTMs, lowfreq = min_freq)

# Find associations for key words
findAssocs(combined_DTMs, "bronco", 0.15)
findAssocs(combined_DTMs, "patriot", 0.15)
findAssocs(combined_DTMs, "cam", 0.15)
findAssocs(combined_DTMs, "newton", 0.15)
findAssocs(combined_DTMs, "peyton", 0.15)
findAssocs(combined_DTMs, "uber", 0.15)
findAssocs(combined_DTMs, "coldplay", 0.15)
findAssocs(combined_DTMs, "beyonc", 0.15)
findAssocs(combined_DTMs, "brunomar", 0.15)

# Tiling

# Clustering, using sparse corpi

## Determine number of clusters through Scree plot
num_clusters = seq(2, 10, 1)
sum_within_ss = sapply(2:10, function(x) sum(kmeans(combined_DTMs, centers = x, iter.max = 5000)$withinss))
plot(num_clusters, sum_within_ss, type = "b")

# Set cluster number
k = 9

## Set distance for clustering
dist <- dist(t(combined_DTMs), method = "euclidian")

## Hierarchical clustering
hierclust <- hclust(dist, method="ward.D2") 
plot(hierclust)
rect.hclust(hierclust, k = k, border = "red")

## K-means clustering
set.seed(123)
kfit <- kmeans(dist, k)
clusplot(as.matrix(dist), kfit$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

# 4. Sentiment Analysis

# Load function file
source("D:\\Foundations of Data Science\\Projects\\Foundations of Data Science - Capstone Project\\scoresentiment.R")

# Import positive and negative keyword lists
pos = scan(file="D:\\Foundations of Data Science\\Projects\\Foundations of Data Science - Capstone Project\\positive-words.txt", what = "character", comment.char = ";")
neg = scan(file="D:\\Foundations of Data Science\\Projects\\Foundations of Data Science - Capstone Project\\negative-words.txt", what = "character", comment.char = ";")

# Clean up all the text entries in the data frame
combined_df$text <- gsub("rt", "", combined_df$text)
combined_df$text <- gsub("@\\w+", "", combined_df$text)
combined_df$text <- gsub("[[:punct:]]", "", combined_df$text)
combined_df$text <- gsub("http\\w+", "", combined_df$text)
combined_df$text <- gsub("[ |\t]{2,}", "", combined_df$text)
combined_df$text <- gsub("^ ", "", combined_df$text)
combined_df$text <- gsub(" $", "", combined_df$text)

# Score all tweet sentiments
combined_df$sentiment <- score.sentiment(combined_df$text, pos, neg)
write.csv(combined_df, file = "D:\\Foundations of Data Science\\Projects\\Foundations of Data Science - Capstone Project\\combined_df.csv")

# Histogram of sentiment
ggplot(data = combined_df, aes(combined_df$sentiment)) + 
  geom_histogram(breaks = seq(-6, 6, by = 1))

# Calculate timeline of Sentiment 
combined_df$hour <- as.POSIXlt(combined_df$created)$hour
combined_df$min <- as.POSIXlt(combined_df$created)$min
combined_df_summary <- ddply(combined_df, c("hour","min"), summarise, N = length(sentiment), avg = mean(sentiment))
combined_df_summary$created <- as.POSIXct(factor(paste0(as.character(combined_df_summary$hour),':',as.character(combined_df_summary$min))) , format="%H:%M")

# Line graph of sentiment over time
ggplot(data = combined_df_summary, aes(x = created, y = avg)) +
  geom_line()

# Geographical view
combined_df$longitude <- as.numeric(combined_df$longitude)
combined_df$latitude <- as.numeric(combined_df$latitude)
ggplot() + 
  borders(databse = "world", regions = ".", colour="gray75", fill="gray75") +
  geom_point(data = combined_df, aes(x = longitude, y = latitude), color = sentiment + 6, size = 3)