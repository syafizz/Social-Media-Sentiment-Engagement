# Load necessary libraries
install.packages(c("ggplot2", "ggthemes", "dplyr", "wordcloud", "tidytext", "stringr"))
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(wordcloud)
library(tidytext)
library(stringr)

# Read the data
df <- read.csv("sentimentdataset.csv")

# Insight data
summary(df)
colnames(df)
head(df)

# Clean the data
df <- df[, -c(1, 2)]

# replace value na to 0
df[is.na(df)] <- 0

# Visualization

# top 50 hashtags
# Tokenize hashtags and count their occurrences
hashtag_counts <- df %>%
  unnest_tokens(word, Hashtags) %>%
  count(word)

# Get the top 50 hashtags
top_50_hashtags <- hashtag_counts %>%
  arrange(desc(n)) %>%
  head(50)

# Create the bar chart
ggplot(top_50_hashtags, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Hashtag", y = "Count", title = "Top 50 Hashtags") +
  theme_minimal()

#top platform
ggplot(df, aes(x = Platform)) + geom_bar() + labs(x = "Name Platform", y = "Count", title = "The Most Platform Engagement") + theme_minimal()

#top word in comment
sentiment_words <- c("good", "great", "excellent", "fantastic", "wonderful", "amazing", "happy", "joyful", "positive", "optimistic", "cheerful", "delighted", "pleased", "satisfied", "grateful", "thankful", "blessed", "lucky", "fortunate", "hopeful", "bad", "terrible", "awful", "horrible", "dreadful", "negative", "pessimistic", "sad", "angry", "frustrated", "disappointed", "upset", "worried", "stressed", "anxious", "fearful", "scared", "lonely", "depressed", "miserable")

# Tokenize the comments and count word frequencies
word_counts <- df %>%
  unnest_tokens(word, Text) %>%
  count(word)

filtered_word_counts <- word_counts %>%
  filter(word %in% sentiment_words)

ggplot(filtered_word_counts, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Word Frequency in Comments")

# sentiment word based on the class
neutral_words <- c("however", "although", "nevertheless", "therefore", "moreover", "furthermore", "besides", "additionally", "consequently", "similarly", "likewise", "otherwise", "instead", "rather", "meanwhile", "simultaneously", "eventually", "finally", "overall", "generally")
positive_words <- c("good", "great", "excellent", "fantastic", "wonderful", "amazing", "happy", "joyful", "positive", "optimistic", "cheerful", "delighted", "pleased", "satisfied", "grateful", "thankful", "blessed", "lucky", "fortunate", "hopeful")
negative_words <- c("bad", "terrible", "awful", "horrible", "dreadful", "negative", "pessimistic", "sad", "angry", "frustrated", "disappointed", "upset", "worried", "stressed", "anxious", "fearful", "scared", "lonely", "depressed", "miserable")

sentiment_lexicon <- data.frame(
  word = c(positive_words, negative_words, neutral_words),
  sentiment = c(rep("positive", length(positive_words)),
                rep("negative", length(negative_words)),
                rep("neutral", length(neutral_words)))
)

# Tokenize the text and join with the sentiment lexicon
sentiment_scores <- text %>%
  unnest_tokens(word, text) %>%
  inner_join(sentiment_lexicon, by = "word")

ggplot(sentiment_lexicon, aes(x = sentiment)) + geom_bar() + labs(x = "Name Platform", y = "Count", title = "The Most Platform Engagement") + theme_minimal()

# the most liked in time
# Sort by like count
most_liked_time <- df %>% 
  arrange(desc(Likes)) %>% 
  head(10)

# Create the time-based line graph
ggplot(most_liked_time, aes(x = Hour, y = Likes)) +
  geom_col(fill = "steelblue") +
  labs(x = "Hour", y = "Number of Likes", title = "Most Liked Posts by Hour") +
  theme_minimal()
