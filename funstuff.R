library(rvest)
library(leaflet)
library(tidytext)
library(xml2)
library(maps)
library(stringdist)
library(tidygeocoder)
library(progress)
base_url <- "https://apnews.com/"

page <- read_html(base_url)

article_links <- page %>% 
  html_nodes("a.Link") %>%
  html_attr("href") %>%
  paste(base_url, ., sep="")


article_data <- data.frame(
  title = character(0),
  location = character(0),
  text = character(0),
  stringsAsFactors = FALSE
)

#init progress bar
pb <- progress_bar$new(
  format = "Scraping [:bar] :percent | ETA :eta",
  total = length(article_links)
)
# Loop through article_links and scrape data
for (i in 1:length(article_links)) {
  link <- article_links[i]
  tryCatch({
    # Read the HTML page
    article_page <- read_html(link)
    # Extract title, location, and text
    article_title <- article_page %>% html_node("h1") %>% html_text()
    article_text <- article_page %>% html_nodes("p") %>% html_text() %>% paste(collapse = " ")
    # Extract location text from the RichTextStoryBody div
    location_div <- article_page %>% html_node("div.RichTextStoryBody.RichTextBody")
    location_text <- location_div %>% html_node("p") %>% html_text()
    # Define a function to extract the location using regular expressions
    extract_location <- function(input_text) {
      location_pattern <- "^(.*?)\\s*\\(AP\\)"
      location_match <- regmatches(input_text, regexec(location_pattern, input_text, perl = TRUE))
      
      if (length(location_match[[1]]) > 0) {
        extracted_location <- location_match[[1]][2]  # Extract the second match
        return(extracted_location)
      } else {
        return("Not found")
      }
    }
    if (!is.null(location_text)) {
      # Extract the location
      extracted_location <- extract_location(location_text)
      cat("Extracted Location:", extracted_location, "\n")
    } else {
      cat("No <p> tag found under 'RichTextStoryBody' div.\n")
      extracted_location <- "Not found"
      pb$tick()
    }
    # Add the scraped data to article_data
    article_data <- rbind(article_data, list(
      title = article_title,
      location = extracted_location,
      text = article_text
    ))
    pb$tick()
    Sys.sleep(5)  # Delay between requests
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "URL:", link, "\n")
    pb$tick()
  })
}

pb$terminate()

