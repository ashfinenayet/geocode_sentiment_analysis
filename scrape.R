library(rvest)
library(leaflet)
library(tidytext)
library(xml2)
library(maps)
library(stringdist)
library(tidygeocoder)
library(progress)
library(furrr)
library(future)

plan(multisession, workers = 3)
base_url <- "https://apnews.com/"

page <- read_html(base_url)

article_links <- page %>% 
  html_nodes("a.Link") %>%
  html_attr("href") %>%
  unique() %>%
  paste(base_url, ., sep="")


article_data <- data.frame(
  title = character(0),
  location = character(0),
  text = character(0),
  stringsAsFactors = FALSE
)

extract_location <- function(input_text) {
  location_pattern <- "^(.*?)\\s*\\(AP\\)"
  location_match <- regmatches(input_text, regexec(location_pattern, input_text, perl = TRUE))
  
  if (length(location_match[[1]]) > 0) {
    extracted_location <- location_match[[1]][2]
    return(extracted_location)
  } else {
    return(NA)  
  }
}


scrape_data <- function(link) {
  tryCatch({
    article_page <- read_html(link)
    article_title <- article_page %>% html_node("h1") %>% html_text(trim = TRUE)
    article_text <- article_page %>% html_nodes("p") %>% html_text(trim = TRUE) %>% paste(collapse = " ")
    
    # Attempt to extract the location
    location_div <- article_page %>% html_node("div.RichTextStoryBody.RichTextBody")
    location_text <- if (!is.null(location_div)) location_div %>% html_node("p") %>% html_text(trim = TRUE) else NA
    
    extracted_location <- if (!is.null(location_text) && location_text != "") {
      extract_location(location_text)
    } else {
      "Location not found"
    }
    
    
    tibble(
      title = article_title,
      location = extracted_location,
      text = article_text,
      link = link  
    )
  }, error = function(e) {
    NULL  # Return NULL in case of an error
  })
}


# parallel processing
article_data <- future_map_dfr(article_links, scrape_data, .progress = TRUE)


