library(shiny)
library(leaflet)
library(wordcloud)
library(bslib)
library(zoo)
library(shinythemes)
library(tidyverse)
library(syuzhet)
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
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel("Sentiment Analysis and Geocoding"),
  sidebarLayout(
    sidebarPanel(
      selectInput("row_select", "Select Article:", choices = names(lat_long$title)),
      selectInput("graph_type", "Select Graph Type:", choices = c("Bar Plot" = "Barplot", "Pie Chart" = "PieChart", "Word Cloud" = "WordCloud",
                                                                  "Emotional Trajectory Plot" = "EmotionalTrajectoryPlot", "Emotional Entropy Plot" = "EmotionalEntropyPlot"))
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "500px"),
      uiOutput("selected_graph")
    )
  )
)

server <- function(input, output, session) {
  # Dynamic update for article selection
  observe({
    updateSelectInput(session, "row_select", choices = lat_long$title, selected = NULL)
  })

  output$map <- renderLeaflet({
    map <- leaflet(lat_long) %>% 
      addTiles() # Start with base tiles
    
    # Apply colors to markers dynamically
    colors <- getColor(lat_long) # Generate color vector
    
    
    
    for(i in 1:nrow(lat_long)) {
      map <- map %>% addAwesomeMarkers(
        lng = jitter(lat_long$longitude[i], amount = 0.01), 
        lat = jitter(lat_long$latitude[i], amount = 0.01),
        popup = as.character(lat_long$summarized_text[i]),
        label = as.character(lat_long$title[i]),
        icon = awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion',
          markerColor = colors[i] # Apply color individually
        ), 
        group = "markers",
        layerId = as.character(lat_long$title[i])
      )
    }
    
    map
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click$id)) {
      updateSelectInput(session, "row_select", selected = click$id)
    }
  })
  

  # Reactive expression for selected rows
  selected_rows <- reactive({
    req(input$row_select)
    lat_long[lat_long$title == input$row_select, ]
  })

  # Reactive expressions for generating graphs
  output$selected_graph <- renderUI({
    req(input$graph_type)
    if (input$graph_type == "Barplot") {
      plotOutput("barplot")
    } else if (input$graph_type == "PieChart") {
      plotOutput("piechart")
    } else if (input$graph_type == "WordCloud") {
      plotOutput("wordcloud")
    } else if (input$graph_type == "EmotionalTrajectoryPlot"){
      plotOutput("emotionaltrajectoryPlot")
    } else if (input$graph_type == "EmotionalEntropyPlot") {
      plotOutput("mixedmessagesPlot")
    }
  })

  # Generate barplot
  output$barplot <- renderPlot({
    req(nrow(selected_rows()) > 0)
    
    # Calculate sums of proportions for selected rows
    sums <- colSums(prop.table(selected_rows()$emotion_clean[, 1:8]))
    
    # Sort the sums
    sums <- sort(sums)
    
    # Create bar plot
    barplot(sums, horiz = TRUE, cex.names = 0.7, las = 1, main = "Emotions in Article", xlab = "Percentage")
  })
  

  # Generate pie chart
  output$piechart <- renderPlot({
    req(nrow(selected_rows()) > 0)
    
    # Calculate sums of proportions for selected rows
    sums <- colSums(prop.table(selected_rows()$emotion_clean[, 1:8]))
    
    percentages <- round(sums * 100, 1)
    
    labels <- paste(names(sums), ": ", percentages, "%", sep = "")
    
    # Create pie chart
    pie(sums, labels = labels, main = "Emotions in Article")
  })
  

  # Generate word cloud
  output$wordcloud <- renderPlot({
    req(nrow(selected_rows()) > 0)
    words <- strsplit(as.character(selected_rows()$clean_text), " ")[[1]]
    word_freq <- table(words)
    wordcloud(names(word_freq), freq = word_freq, random.order = FALSE)
  })
  
  # Generate sentiment plot
  output$emotionaltrajectoryPlot <- renderPlot({
    req(nrow(selected_rows()) > 0)
    text_vector <- unlist(str_split(selected_rows()$text, pattern = "\\.\\s*"))
    sentiments <- get_sentiment(text_vector, method = "syuzhet")
    simple_plot(sentiments)
  })
  output$mixedmessagesPlot <- renderPlot({
    req(nrow(selected_rows()) > 0)
    sents <- get_sentences(selected_rows()$text)
    test <- lapply(sents, mixed_messages)
    entropes <- do.call(rbind, test)
    out <- data.frame(entropes, sents, stringsAsFactors = F)
    simple_plot(out$entropy,title = "Emotional Entropy",legend_pos = "top")
    
  })
}

shinyApp(ui, server)
