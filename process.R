library(tidyverse)
library(tidygeocoder)
library(leaflet)
library(tm)
library(syuzhet)
library(tidytext)
library(topicmodels)
# Ensure article_data is a tibble and remove rows with NA locations and duplicates
df2 <- article_data %>%
  filter(!is.na(location)) %>%
  distinct() %>%
  as_tibble()

# Preprocess text data: convert to lowercase, remove punctuation, whitespaces, stopwords
corpus <- Corpus(VectorSource(df2$text)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, stopwords('english'))

# Add cleaned text and calculate sentiment
df2 <- df2 %>%
  mutate(
    clean_text = sapply(corpus, as.character),
    sentiment_score_clean = get_sentiment(clean_text),
    emotion_clean = get_nrc_sentiment(clean_text)
  )

# Geocode locations
df2 <- df2 %>%
  geocode(location, method = 'osm', lat = latitude, long = longitude)

# Summarize text
updated_summarize <- function(text) {
  sentences <- strsplit(text, "\\. ")[[1]]
  summary_length <- ifelse(length(sentences) > 2, min(5, length(sentences) - 2), length(sentences))
  paste(sentences[1:summary_length], collapse = ". ") %>% paste0(., ".")
}

df2 <- df2 %>%
  mutate(summarized_text = sapply(text, updated_summarize))

# Expand emotion_clean and write to CSV
df2 %>%
  unnest(emotion_clean) %>%
  write_csv("lat_long.csv")



