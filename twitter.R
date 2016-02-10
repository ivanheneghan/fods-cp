# Install packages
# install.packages("base64enc")
# install.packages("twitteR")
# install.packages("RCurl")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("SnowballC")
# install.packages("fpc")
# install.packages("RColorBrewer")
# install.packages("ape")

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
library(scales)
library(knitr)
library(ape)

# 1. Extract data from Twitter

# Set up Twitter API credentials
# consumer_key <- "jnUHsr3bg5QscbQcIMF9xei1A"
# consumer_secret <- "6GJQGIUA63kfO8uIKz8I6fQI7BA9KTFPXDofp6HxvxEJF8Wu4d"
# access_token <- "14611173-xXtSpD44S8A1zWv4Qzit6DwdXOuIC4hm57WvZycHA"
# access_secret <- "AMxapPQJZnadlTqns1JyoUBquVNfov6AqEt9Pn1z3jpFI"

# Create Twitter connection
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Get Twitter data for each of the 4 main hashtags - 10k tweets for each, from 6/2 onwards, English language tweets only
# superbowl <- searchTwitter("#superbowl", n = 10000, lang = "en", since = "2015-02-06")
# superbowl50 <- searchTwitter("#superbowl50", n = 10000, lang = "en", since = "2015-02-06")
# nfl <- searchTwitter("#nfl", n = 10000, lang = "en", since = "2015-02-06")
# sb50 <- searchTwitter("#sb50", n = 10000, lang = "en", since = "2015-02-06")

# 2. Manipulate & clean extracted data

# Strip retweets from extracted Twitter data
superbowl_no_rt = strip_retweets(superbowl)
superbowl50_no_rt = strip_retweets(superbowl50)
nfl_no_rt = strip_retweets(nfl)
sb50_no_rt = strip_retweets(sb50)

# Create data frames from Twitter data
superbowl_df <- twListToDF(superbowl_no_rt)
superbowl50_df <- twListToDF(superbowl50_no_rt)
nfl_df <- twListToDF(nfl_no_rt)
sb50_df <- twListToDF(sb50_no_rt)

write.csv(superbowl_df, file = "superbowl_df.csv")
write.csv(superbowl50_df, file = "superbowl50_df.csv")
write.csv(nfl_df, file = "nfl_df.csv")
write.csv(sb50_df, file = "sb50_df.csv")

# Create combined data frame
combined_df <- rbind(superbowl_df, superbowl50_df)
combined_df <- rbind(combined_df, nfl_df)
combined_df <- rbind(combined_df, sb50_df)

# Create corpus
combined_corpus <- Corpus(VectorSource(combined_df$text))

# Perform Corpus transformations
# Convert all text to lowercase
combined_corpus <- tm_map(combined_corpus, content_transformer(tolower))
# Remove punctuation
combined_corpus <- tm_map(combined_corpus, removePunctuation)
# Remove numbers
combined_corpus <- tm_map(combined_corpus, removeNumbers)
# Remove URLs
combined_corpus <- tm_map(combined_corpus, function(x) gsub("http[[:alnum:]]*", "", x))
# Remove any stopwords
combined_corpus <- tm_map(combined_corpus, removeWords, stopwords("english"))
# Stem the text
combined_corpus <- tm_map(combined_corpus, stemDocument)
# Remove any whitespace
combined_corpus <- tm_map(combined_corpus, stripWhitespace)

# Create DTM & Sparse DTM
combined_corpus <- tm_map(combined_corpus, PlainTextDocument)
combined_DTM <- DocumentTermMatrix(combined_corpus)
combined_DTMs <- removeSparseTerms(combined_DTM, 0.99)

dim(combined_DTM)
dim(combined_DTMs)

# 3. Find frequent words and associations

# Define frequency variables
min_freq <- 100

# Frequent words
findFreqTerms(combined_DTMs, lowfreq = min_freq)

# Plot frequent words
term_freq <- colSums(as.matrix(combined_DTMs))
term_freq <- subset(term_freq, term_freq >= min_freq)
freq_words_df <- data.frame(term = names(term_freq), freq = term_freq)
ggplot(data = freq_words_df, aes(x = reorder(term, freq), y = freq, colour = freq)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Frequency of Most-Used Terms") +
  xlab("Terms") +
  ylab("Frequency") + 
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10))
  
# Find associations for key words

# Set association variable
assoc <- 0.25

# Find associations
broncos_assoc <- findAssocs(combined_TDM, "broncos", assoc)
avos_assoc <- findAssocs(combined_TDM, "avosinspace", assoc)
cam_assoc <- findAssocs(combined_DTM, "cam", assoc)
patriots_assoc <- findAssocs(combined_DTM, "patriots", assoc)
panther_assoc <- findAssocs(combined_DTM, "panther", assoc)
newton_assoc <- findAssocs(combined_DTM, "newton", assoc)
uber_assoc <- findAssocs(combined_DTM, "uber", assoc)
beyonce_assoc <- findAssocs(combined_DTM, "beyonc", assoc)
peyton_assoc <- findAssocs(combined_DTM, "peyton", assoc)
coldplay_assoc <- findAssocs(combined_DTM, "coldplay", assoc)

# Plotting associations
broncos_associations_df <- do.call(rbind, Map(function(d, n) cbind.data.frame(xterm=if (length(d)>0) names(d) else NA, cor=if(length(d)>0) d else NA, term=n), panther_assoc, names(broncos_assoc)))
ggplot(data = broncos_associations_df, aes(x = reorder(xterm, -cor), y = cor)) +
  geom_point(size = 3, aes(colour = cor)) + 
  ggtitle("Words Associated with Broncos") +
  xlab("Terms") +
  ylab("Association") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10)) +
  scale_colour_gradientn(colours=c("red", "blue")) 

avos_associations_df <- do.call(rbind, Map(function(d, n) cbind.data.frame(xterm=if (length(d)>0) names(d) else NA, cor=if(length(d)>0) d else NA, term=n), panther_assoc, names(avos_assoc)))
ggplot(data = avos_associations_df, aes(x = reorder(xterm, -cor), y = cor)) +
  geom_point(size = 3, aes(colour = cor)) + 
  ggtitle("Words Associated with Avos") +
  xlab("Terms") +
  ylab("Association") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10)) +
  scale_colour_gradientn(colours=c("red", "blue")) 

cam_associations_df <- do.call(rbind, Map(function(d, n) cbind.data.frame(xterm=if (length(d)>0) names(d) else NA, cor=if(length(d)>0) d else NA, term=n), cam_assoc, names(cam_assoc)))
ggplot(data = cam_associations_df, aes(x = reorder(xterm, -cor), y = cor)) +
  geom_point(size = 3, aes(colour = cor)) + 
  ggtitle("Words Associated with Can") +
  xlab("Terms") +
  ylab("Association") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10)) +
  scale_colour_gradientn(colours=c("red", "blue")) 

patriot_associations_df <- do.call(rbind, Map(function(d, n) cbind.data.frame(xterm=if (length(d)>0) names(d) else NA, cor=if(length(d)>0) d else NA, term=n), patriot_assoc, names(patriot_assoc)))
ggplot(data = patriot_associations_df, aes(x = reorder(xterm, -cor), y = cor)) +
  geom_point(size = 3, aes(colour = cor)) + 
  ggtitle("Words Associated with Patriot") +
  xlab("Terms") +
  ylab("Association") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10)) +
  scale_colour_gradientn(colours=c("red", "blue")) 

panther_associations_df <- do.call(rbind, Map(function(d, n) cbind.data.frame(xterm=if (length(d)>0) names(d) else NA, cor=if(length(d)>0) d else NA, term=n), panther_assoc, names(panther_assoc)))
ggplot(data = panther_associations_df, aes(x = reorder(xterm, -cor), y = cor)) +
  geom_point(size = 3, aes(colour = cor)) + 
  ggtitle("Words Associated with Panther") +
  xlab("Terms") +
  ylab("Association") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10)) +
  scale_colour_gradientn(colours=c("red", "blue")) 

newton_associations_df <- do.call(rbind, Map(function(d, n) cbind.data.frame(xterm=if (length(d)>0) names(d) else NA, cor=if(length(d)>0) d else NA, term=n), newton_assoc, names(newton_assoc)))
ggplot(data = newton_associations_df, aes(x = reorder(xterm, -cor), y = cor)) +
  geom_point(size = 3, aes(colour = cor)) + 
  ggtitle("Words Associated with Newton") +
  xlab("Terms") +
  ylab("Association") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10)) +
  scale_colour_gradientn(colours=c("red", "blue")) 

uber_associations_df <- do.call(rbind, Map(function(d, n) cbind.data.frame(xterm=if (length(d)>0) names(d) else NA, cor=if(length(d)>0) d else NA, term=n), uber_assoc, names(uber_assoc)))
ggplot(data = uber_associations_df, aes(x = reorder(xterm, -cor), y = cor)) +
  geom_point(size = 3, aes(colour = cor)) + 
  ggtitle("Words Associated with Uber") +
  xlab("Terms") +
  ylab("Association") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10)) +
  scale_colour_gradientn(colours=c("red", "blue")) 

beyonce_associations_df <- do.call(rbind, Map(function(d, n) cbind.data.frame(xterm=if (length(d)>0) names(d) else NA, cor=if(length(d)>0) d else NA, term=n), beyonce_assoc, names(beyonce_assoc)))
beyonce_associations_df = beyonce_associations_df[-1,]
ggplot(data = beyonce_associations_df, aes(x = reorder(xterm, -cor), y = cor)) +
  geom_point(size = 3, aes(colour = cor)) + 
  ggtitle("Words Associated with Beyonce") +
  xlab("Terms") +
  ylab("Association") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10)) +
  scale_colour_gradientn(colours=c("red", "blue")) 

peyton_associations_df <- do.call(rbind, Map(function(d, n) cbind.data.frame(xterm=if (length(d)>0) names(d) else NA, cor=if(length(d)>0) d else NA, term=n), peyton_assoc, names(peyton_assoc)))
ggplot(data = peyton_associations_df, aes(x = reorder(xterm, -cor), y = cor)) +
  geom_point(size = 3, aes(colour = cor)) + 
  ggtitle("Words Associated with Peyton") +
  xlab("Terms") +
  ylab("Association") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10)) +
  scale_colour_gradientn(colours=c("red", "blue")) 

coldplay_associations_df <- do.call(rbind, Map(function(d, n) cbind.data.frame(xterm=if (length(d)>0) names(d) else NA, cor=if(length(d)>0) d else NA, term=n), coldplay_assoc, names(coldplay_assoc)))
ggplot(data = coldplay_associations_df, aes(x = reorder(xterm, -cor), y = cor)) +
  geom_point(size = 3, aes(colour = cor)) + 
  ggtitle("Words Associated with Coldplay") +
  xlab("Terms") +
  ylab("Association") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10)) +
  scale_colour_gradientn(colours=c("red", "blue")) 

# 4. Wordcloud, using corpus
wordcloud(combined_text_corpus, min.freq = min_freq, scale = c(2, 0.5), colors = brewer.pal(8, "Paired"),  random.color = TRUE, random.order = FALSE, max.words = 250)

# 5. Clustering, using sparse corpi

## Determine number of clusters through Scree plot
num_clusters = seq(2, 10, 1)
sum_within_ss = sapply(2:10, function(x) sum(kmeans(combined_DTMs, centers = x, iter.max = 50000)$withinss))
plot(num_clusters, sum_within_ss, type = "b")

# Set cluster number
k = 8

## Set distance for clustering
d <- dist(t(combined_DTMs), method = "euclidian")

## Hierarchical clustering
mypal <- brewer.pal(k, "Set1")
fit <- hclust(d, method="ward.D2")
groups <- cutree(hierclust, k)
plot(as.phylo(hierclust), type = "fan", tip.color = mypal[groups])


## K-means clustering
# set.seed(0)
# kfit <- kmeans(d, k)
# plot(kfit)
# clusplot(as.matrix(distance), kfit$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

# 6. Sentiment Analysis

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

# Histogram of sentiment
ggplot(data = combined_df, aes(combined_df$sentiment)) + 
  geom_histogram(binwidth=1, origin = -6.5, color="darkblue", fill="lightblue") +
  ggtitle("Sentiment Histogram") +
  scale_x_continuous(breaks=seq(-6, 6, by = 1)) +
  scale_y_sqrt() +
  xlab("Sentiment") +
  ylab("Number of Terms") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10))

# Calculate timeline of Sentiment 
combined_df$hour <- as.POSIXlt(combined_df$created)$hour
combined_df$min <- as.POSIXlt(combined_df$created)$min
combined_df_summary <- ddply(combined_df, c("hour","min"), summarise, vol = length(sentiment), avg = mean(sentiment))
combined_df_summary$created <- as.POSIXct(factor(paste0(as.character(combined_df_summary$hour),':',as.character(combined_df_summary$min))) , format="%H:%M")

# Line graph of sentiment over time
ggplot(data = combined_df_summary, aes(x = created, y = avg)) +
  geom_line(colour="lightblue", size = 1) +
  scale_x_datetime(breaks = date_breaks("4 hour"), labels=date_format("%H:%M")) +
  ggtitle("Sentiment over a 24 Hour Period") +
  xlab("Time of Day") +
  ylab("Sentiment") +
  theme(plot.title = element_text(size=14, face = "bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(face = "bold", size=10),
        axis.text.y = element_text(face = "bold", size=10))

# 7. Heatmap
combined_df_subset <- subset(combined_df, combined_df$favoriteCount > 50)
combined_df_subset$favoriteCountBucket <- cut(combined_df_subset$favoriteCount, breaks = seq(50, 1800, by = 50))
ggplot(combined_df_subset, aes(x = favoriteCountBucket, y = screenName, fill = sentiment)) + 
  geom_tile() +
  ggtitle("Sentiment of the Most Popular Tweets (and Who Tweeted)") +
  xlab("Number of Times Favourited By Others") +
  ylab("Username") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10))

# 8. Geographical mapping of tweets (and associated sentiment)
combined_df$longitude <- as.numeric(combined_df$longitude)
combined_df$latitude <- as.numeric(combined_df$latitude)
ggplot() + 
  borders(database = "world", regions = ".", colour="gray75", fill="gray75") +
  geom_point(data = combined_df, aes(x = longitude, y = latitude, color = sentiment), size = 3) + 
  ggtitle("Tweeter Locations") +
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10)) +
  scale_colour_gradientn(colours=c("red", "blue"))

save.image("D:\\Foundations of Data Science\\Projects\\Foundations of Data Science - Capstone Project\\capstone.RData")
