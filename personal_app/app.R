library(shiny)

# Define UI
ui <- fluidPage(
  # Add dropdown menu for OTT media services
  selectInput("ott_service", "Which platform do you use most commonly to watch K-Dramas?",
              choices = c("Netflix", "YouTube", "Amazon Prime", "Disney+", "YouTube Premium", "iQiyi")),
  
  # Add placeholder for displaying information
  textOutput("ott_info")
)

# Define server logic
server <- function(input, output) {
  # Define reactive expression to filter data based on OTT service
  filtered_data <- reactive({
    switch(input$ott_service,
           "Netflix" = 69.90,
           "YouTube" = 62.60,
           "Amazon Prime" = 29.30,
           "Disney+" = 23.20,
           "YouTube Premium" = 18.10,
           "iQiyi" = 15.20)
  })
  
  # Display information based on selected OTT service
  output$ott_info <- renderText({
    paste("If you use", input$ott_service, "you would be among", filtered_data(), "% of global audiences.")
  })
}

# Run the Shiny app
shinyApp(ui, server)
