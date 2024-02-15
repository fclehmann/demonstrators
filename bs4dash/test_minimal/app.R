# app.R
library(shiny)

# Source the function file
source("multiply_function.R")

# Define UI
ui <- fluidPage(
  titlePanel("Slider Value Processor"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider1", "Slider:", min = 1, max = 10, value = 5), 
      sliderInput("slider2", "Slider:", min = 1, max = 10, value = 5)
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to process slider value
  processed_value <- reactive({
    vallist <- list(val1 = input$slider1, val2 = input$slider2)
    adding_sliders(vallist)
  })
  
  # Output the processed value
  output$result <- renderText({
    paste("Processed value:", processed_value())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
