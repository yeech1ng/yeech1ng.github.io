library(shiny)
library(wordcloud2)
library(tidyverse)
library(tm)

# UI
ui <- fluidPage(
  titlePanel("Word Clouds of Drama Tags"),
  sidebarLayout(
    sidebarPanel(
      selectInput("language", "Select Language:",
                  choices = c("Korean", "Thai", "Japanese"))
    ),
    mainPanel(
      wordcloud2Output("wordcloud")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Read data from CSV files
  general_kdrama <- read.csv("kor_drama.csv")
  general_tdrama <- read.csv("tha_drama.csv")
  general_jdrama <- read.csv("jap_drama.csv")
  
  # Convert year to numeric 
  suppressWarnings({
    general_kdrama$year <- as.numeric(as.character(general_kdrama$year))
    general_tdrama$year <- as.numeric(as.character(general_tdrama$year))
    general_jdrama$year <- as.numeric(as.character(general_jdrama$year))
  })
  
  # Function to preprocess data and create word cloud
  create_wordcloud <- function(data) {
    tags <- data %>% 
      filter(year >= 2018) %>% 
      select(tags)
    
    docs <- Corpus(VectorSource(tags))
    docs <- docs %>% 
      tm_map(removePunctuation) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeWords, stopwords("english"))
    
    dtm <- TermDocumentMatrix(docs)
    matrix <- as.matrix(dtm)
    words <- sort(rowSums(matrix), decreasing=TRUE)
    tags_df <- data.frame(word = names(words), freq = words)
    
    return(wordcloud2(slice_max(tags_df, order_by = freq, n = 10000), 
                      size = 1.2, color = 'random-dark', shape = 'circle'))
  }
  
  # Render word cloud based on selected language
  output$wordcloud <- renderWordcloud2({
    if (input$language == "Korean") {
      create_wordcloud(general_kdrama)
    } else if (input$language == "Thai") {
      create_wordcloud(general_tdrama)
    } else if (input$language == "Japanese") {
      create_wordcloud(general_jdrama)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
