library(shiny)
library(leaflet)
library(wordcloud)
library(bslib) 
library(tidyverse)
library(syuzhet)
lat_long_csv <- read_csv('lat_long.csv')
getColor <- function(lat_long_csv) {
  sapply(lat_long_csv$sentiment_score_clean, function(sentiment_score_clean) {
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
  markerColor = getColor(lat_long_csv)
)
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel("Sentiment Analysis and Geocoding"),
  sidebarLayout(
    sidebarPanel(
      selectInput("row_select", "Select Article:", choices = names(data()$title)),
      selectInput("graph_type", "Select Graph Type:", choices = c("Bar Plot" = "Barplot", "Pie Chart" = "PieChart", "Word Cloud" = "WordCloud",
                                                                  "Emotional Trajectory Plot" = "EmotionalTrajectoryPlot", "Emotional Entropy Plot" = "EmotionalEntropyPlot")),
      #uiOutput("conditional_button")
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "500px"),
      uiOutput("selected_graph"), 
      dataTableOutput('table') #LDA topic model plot
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactiveFileReader(intervalMillis = 60000, session, filePath = 'lat_long.csv', read.csv)
  # Dynamic update for article selection
  observe({
    updateSelectInput(session, "row_select", choices = data()$title, selected = NULL)
  })
  
  output$map <- renderLeaflet({
    map <- leaflet(data()) %>% 
      addTiles() # Start with base tiles
    
    # Apply colors to markers dynamically
    colors <- getColor(data()) # Generate color vector
    
    
    
    for(i in 1:nrow(data())) {
      map <- map %>% addAwesomeMarkers(
        lng = jitter(data()$longitude[i], amount = 0.01), 
        lat = jitter(data()$latitude[i], amount = 0.01),
        popup = as.character(data()$summarized_text[i]),
        label = as.character(data()$title[i]),
        icon = awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion',
          markerColor = colors[i] # Apply color individually
        ), 
        group = "markers",
        layerId = as.character(data()$title[i])
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
    data()[data()$title == input$row_select, ]
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
    
    # Select the relevant columns (assuming they are named emotion1, emotion2, ..., emotion8)
    emotions_df <- selected_rows()[, c("anger", "anticipation", "fear", "joy", 
                                       "sadness", "surprise", "trust")]
    
    # Calculate the sum of each emotion column
    emotion_sums <- colSums(emotions_df, na.rm = TRUE)
    
    emotion_props <- emotion_sums / sum(emotion_sums)
    
    emotion_props_sorted <- sort(emotion_props, decreasing = F)
    # Sort the sums
    
    # Create a bar plot
    barplot(emotion_props_sorted, horiz = TRUE, cex.names = 0.7, las = 1,
            main = "Emotions in Article", xlab = "Percentage")
  })
  
  
  # Generate pie chart
  output$piechart <- renderPlot({
    req(nrow(selected_rows()) > 0)
    
    # Extract the relevant emotion columns from the selected rows
    emotions_df <- selected_rows()[, c("anger", "anticipation", "fear", "joy", 
                                       "sadness", "surprise", "trust")]
    
    # Calculate the sums of each emotion
    sums <- colSums(emotions_df, na.rm = TRUE)
    
    # Convert sums to percentages
    percentages <- round(sums / sum(sums) * 100, 1)
    
    # Create labels for the pie chart slices
    labels <- paste(names(sums), ": ", percentages, "%", sep = "")
    
    # Create the pie chart with labeled slices
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
    
    output$bubblePlot <- renderPlot({
      req(nrow(selected_rows()) > 0)
      req(input$emotionColumn)
      ggplot(data(), aes(x = latitude, y = longitude, size = .data[[input$emotionColumn]])) + 
        geom_point()
    })
    
  })
  
  
}
shinyApp(ui, server)
