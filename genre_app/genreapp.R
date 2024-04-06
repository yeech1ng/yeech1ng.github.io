library(shiny)
library(wordcloud2)
library(tidyverse)
library(tm)

# Define UI
ui <- fluidPage(
  titlePanel("Drama Word Clouds"),
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

# Define server logic
server <- function(input, output) {
  
  # Load data and preprocess based on selected language
  drama_data <- reactive({
    switch(input$language,
           "Korean" = general_kdrama %>% filter(year >= 2018),
           "Thai" = general_tdrama %>% filter(year >= 2018),
           "Japanese" = general_jdrama %>% filter(year >= 2018))
  })
  
  # Generate word cloud based on selected language
  output$wordcloud <- renderWordcloud2({
    tags <- drama_data() %>% select(tags) 
    
    docs <- Corpus(VectorSource(tags))
    docs <- docs %>% 
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) 
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    
    dtm <- TermDocumentMatrix(docs)
    matrix <- as.matrix(dtm)
    words <- sort(rowSums(matrix), decreasing=TRUE)
    tags_df <- data.frame(word = names(words), freq = words)
    
    wordcloud2(slice_max(tags_df, order_by = freq, n = 100), size = 1.2, color = 'random-dark', shape = 'circle')
  })
}

# Define Shiny app
shinyApp(ui = ui, server = server)
