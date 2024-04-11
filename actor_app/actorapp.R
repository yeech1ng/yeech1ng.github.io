library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

# UI definition
ui <- fluidPage(
  titlePanel("Correlation Plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Select Plot Type:",
                  choices = c("Korean" = "kor", "Thai" = "tha", "Japanese" = "jap"))
    ),
    mainPanel(
      plotlyOutput("correlation_plot")
    )
  )
)

# Server logic
server <- function(input, output) {
  # List files in the 'actor_app' directory
  observe({
    files <- list.files("actor_app")
    print(files)
  })
  
  # Read data from CSV files
  kreviews <- read.csv("kor_user_reviews.csv")
  treviews <- read.csv("tha_user_reviews.csv")
  jreviews <- read.csv("jap_user_reviews.csv")
  
  # Function to create correlation plot
  create_plot <- function(data, language) {
    color_palette <- c("#66c2a5", "#fc8d62", "#8da0cb")  # Set2 color palette
    
    ggplot(data, aes(x = acting_cast, y = overall, color = language)) +
      geom_jitter() +
      geom_smooth() +
      scale_color_manual(values = color_palette) +  # Set manual color scale
      ggtitle(paste("Correlation between Acting Cast Ratings and Overall Ratings for", language, "dramas")) +
      xlab("Acting Cast Ratings") +
      ylab("Overall Ratings")
  }
  
  # Render the correlation plot based on selected plot type
  output$correlation_plot <- renderPlotly({
    if (input$plot_type == "kor") {
      plot <- create_plot(kreviews, "Korean")
    } else if (input$plot_type == "tha") {
      plot <- create_plot(treviews, "Thai")
    } else if (input$plot_type == "jap") {
      plot <- create_plot(jreviews, "Japanese")
    }
    ggplotly(plot)
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)
