library(tibble)
library(dplyr)
library(tidygeocoder)
library(leaflet)
library(tm)
library(syuzhet)
library(tidyr)
library(tidyverse)
library(tidytext)
library(parallel)
library(purrr)



as_tibble(article_data)

df2 <- article_data[!is.na(article_data$location),]

df2 <- df2[!duplicated(df2), ]

df2 <- as_tibble(df2)

corpus <- Corpus(VectorSource(df2$text)) 

# Convert to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# Remove white spaces
corpus <- tm_map(corpus, stripWhitespace)

# Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords('english'))


df2$clean_text <- sapply(corpus, as.character)

  
df2$sentiment_score_clean <- get_sentiment(df2$clean_text)
df2$emotion_clean <- get_nrc_sentiment(df2$clean_text)
##### emotional ambiguity ----



# geocode locations
lat_long <- df2 %>% geocode(location, method = 'osm', lat = latitude, long = longitude)



## summary

# Print the summary

updated_summarize <- function(text) {
  sentences <- strsplit(text, "\\. ")[[1]]  # Splitting the text into sentences
  if (length(sentences) > 2) {
    sentences <- sentences[3:length(sentences)]  # Skip the first two sentences
  }
  summary_length <- min(5, length(sentences))  
  return(paste(sentences[1:summary_length], collapse = ". ") %>% paste0(., "."))
}

lat_long$summarized_text <- sapply(lat_long$text, updated_summarize)

## write to csv
lat_long <- unnest(lat_long, emotion_clean)
write_csv(lat_long, "lat_long.csv")
