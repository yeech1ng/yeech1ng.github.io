library(shiny)
library(rsconnect)

# UI for the webpage
ui <- fluidPage(
  titlePanel("Quiz Time! ^_^"),
  sidebarLayout(
    sidebarPanel(
      # Question with increased font size
      tags$h2("What is the most popular genre among K-Dramas?", style = "font-size: 20px;"),
      # Adding GIF
      tags$img(src = "squid-game.gif", height = "150px", width = "300px"), # Change "path_to_gif.gif" to the actual path of your GIF
      radioButtons("q1", "Select your answer:",
                   choices = list("A. Horror", "B. Romance", "C. Youth", "D. Drama")),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      # Feedback
      textOutput("feedback")
    )
  )
)

# Server logic
server <- function(input, output) {
  observeEvent(input$submit, {
    # Evaluate user's answer
    if (input$q1 == "B. Romance") {
      output$feedback <- renderText("You're correct! From 2018 to 2023, 788 K-Dramas were labelled as Romance dramas.")
    } else {
      output$feedback <- renderText("Try again!")
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

