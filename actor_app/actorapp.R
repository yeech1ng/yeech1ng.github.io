library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

# UI definition
ui <- fluidPage(
  titlePanel("Korean, Thai, and Japanese Dramas Correlation Plots"),
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
    palette <- brewer.pal(3, "Set2")  # Get color palette
    
    ggplot(data, aes(x = acting_cast, y = overall)) +
      geom_jitter(aes(color = language), show.legend = FALSE) +
      geom_smooth(aes(color = language), show.legend = FALSE) +
      ggtitle(paste("Acting Cast Ratings against Overall Ratings", language, "dramas")) +
      xlab("Acting Cast Ratings") +
      ylab("Overall Ratings") +
      scale_color_manual(values = palette)  # Set color scale
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
