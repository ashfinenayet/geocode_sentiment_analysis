library(tibble)
library(dplyr)
library(tidygeocoder)
library(leaflet)
library(tm)
library(syuzhet)
library(tidyr)
library(lexRankr)
library(spacyr)
library(textfeatures)
library(tidyverse)
library(tidytext)
library(textrank)
#library(LSAfun)


as_tibble(article_data)

df2 <- article_data[!(article_data$location == "Not found"),]

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
#df2$sentiment_score_bing <- df2 %>%
  
df2$sentiment_score_clean <- get_sentiment(df2$clean_text)
df2$emotion_clean <- get_nrc_sentiment(df2$clean_text)
df2$emotion_clean$anger
##### emotional ambiguity ----

# geocode locations
lat_long <- df2 %>% geocode(location, method = 'osm', lat = latitude, long = longitude)
### extract prototypical sentences for 5 sentence summary
article_sentences <- tibble(text = df2$text[[1]]) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentence_id = row_number()) %>%
  select(sentence_id, sentence)

# Print the article_sentences tibble
print(article_sentences)

# Tokenize the sentences into words, remove stop words
article_words <- article_sentences %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")  # Ensure you're using get_stop_words() to fetch the stop words data frame

# Ensure the 'textrank_sentences' function is used correctly
# 'data' should be your sentences, and 'terminology' should be the important words or phrases, usually extracted from the text.
# This might need adjustments based on your specific requirements or the textrank package you're using.

# Since 'textrank_sentences' function isn't a standard part of loaded packages, ensure you have it available, 
# possibly from the 'textrank' package or another source.
# This example assumes 'textrank_sentences' is available and works with the given parameters.
article_summary <- textrank_sentences(data = article_sentences, terminology = article_words)



df <- df %>%
  mutate(summarized_article = map(article_text, function(article) {
    # Tokenize the article into sentences
    sentences <- tibble(text = article) %>%
      unnest_tokens(sentence, text, token = "sentences") %>%
      mutate(sentence_id = row_number())

    # Apply TextRank to summarize the article
    summary <- textrank_sentences(data = sentences, directed = FALSE)

    # Extract the top-ranked sentences as the summary (adjust the 'n' value based on your preference)
    summarized_text <- summary %>%
      top_n(5, wt = rank) %>%
      arrange(rank) %>%
      summarise(summary = paste(sentence, collapse = " ")) %>%
      pull(summary)

    return(summarized_text)
  }))

write_csv(lat_long, "lat_long")
# Print the summary
print(article_summary)
library(ggplot2)
article_summary[["sentences"]] %>%
  ggplot(aes(textrank_id, textrank, fill = textrank_id)) +
  geom_col() +
  theme_minimal() +
  scale_fill_viridis_c() +
  guides(fill = "none") +
  labs(x = "Sentence",
       y = "TextRank score",
       title = "Article")

updated_summarize <- function(text) {
  sentences <- strsplit(text, "\\. ")[[1]]  # Splitting the text into sentences
  if (length(sentences) > 2) {
    sentences <- sentences[3:length(sentences)]  # Skip the first two sentences
  }
  summary_length <- min(5, length(sentences))  
  return(paste(sentences[1:summary_length], collapse = ". ") %>% paste0(., "."))
}

lat_long$summarized_text <- sapply(lat_long$text, updated_summarize)
##### getColor Function and leaflet ----

getColor <- function(lat_long) {
  sapply(lat_long$sentiment_score_clean, function(sentiment_score_clean) {
    if (sentiment_score_clean < 0) {
      return("red")
    } else {
      return("green")
    }
  })
}
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(lat_long)
)

m <- leaflet(data = lat_long) %>%
  addTiles() %>%
  addAwesomeMarkers(
    popup = ~as.character(summarized_text),
    icon = icons,
    label = ~as.character(title),
    labelOptions = labelOptions(noHide = F, direction = 'auto')
  )

m
