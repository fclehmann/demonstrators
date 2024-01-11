library(shiny)
library(ggplot2)

ui <- fluidPage(
  plotOutput("plot1", click = "plot_click", hover = "plot_hover"),
  verbatimTextOutput("click_info"),
  verbatimTextOutput("mouse_position")
)

server <- function(input, output, session) {
  clickedPoints <- reactiveValues(
    click_coords = matrix(0, ncol = 2, nrow = 0),
    line_exists = FALSE
  )
  
  # Reactive value to store mouse position
  mouse_position <- reactiveVal(NULL)
  
  output$plot1 <- renderPlot({
    base_plot <- ggplot() + geom_point(data = mtcars, aes(x = wt, y = mpg))
    
    # Connect the last clicked point to the mouse pointer position with a dashed line
    if (!clickedPoints$line_exists && nrow(clickedPoints$click_coords) < 2 && nrow(clickedPoints$click_coords) > 0 && !is.null(mouse_position())) {
      base_plot <- base_plot +
        geom_point(aes(
          x = clickedPoints$click_coords[1, 1],
          y = clickedPoints$click_coords[1, 2]),
          color = "red",
          size = 3
        )  +
        geom_line(aes(
          x = c(clickedPoints$click_coords[1, 1], mouse_position()$x),
          y = c(clickedPoints$click_coords[1, 2], mouse_position()$y)
          ),
          color = "blue",
          linetype = "dashed"
        )
    }
    
    if (nrow(clickedPoints$click_coords) >= 2) {
      base_plot <- base_plot +
        geom_point(aes(
          x = clickedPoints$click_coords[1, 1],
          y = clickedPoints$click_coords[1, 2]),
          color = "red",
          size = 3
        )  +
        geom_point(aes(
          x = clickedPoints$click_coords[2, 1],
          y = clickedPoints$click_coords[2, 2]),
          color = "red",
          size = 3
        ) +
        geom_line(aes(
          x = c(clickedPoints$click_coords[1, 1], clickedPoints$click_coords[2, 1]),
          y = c(clickedPoints$click_coords[1, 2], clickedPoints$click_coords[2, 2])
        ),
        color = "blue",
        linetype = "dashed"
        )
      clickedPoints$line_exists <- TRUE
    }
    
    base_plot
  })
  
  observe({
    # Update mouse position when hovering
    if (!is.null(input$plot_hover)) {
      mouse_position(input$plot_hover)
    }
  })
  
  observeEvent(input$plot_click, {
    click_data <- input$plot_click
    
    if (clickedPoints$line_exists == TRUE) {
      clickedPoints$click_coords <- matrix(0, ncol = 2, nrow = 0)
      clickedPoints$line_exists = FALSE
    }
    
    if (!is.null(click_data)) {
      clickedPoints$click_coords <- rbind(clickedPoints$click_coords, c(click_data$x, click_data$y))
    }
  })
  
  output$click_info <- renderPrint({
    # Display the full matrix of click coordinates
    clickedPoints$click_coords
  })
  
  output$mouse_position <- renderPrint({
    # Display the most recent mouse position
    if (!is.null(mouse_position())) {
      paste0("Mouse Position: (", mouse_position()$x, ", ", mouse_position()$y, "), num clicks: ", nrow(clickedPoints$click_coords), ", line_exists:", clickedPoints$line_exists)
    }
  })
}

shinyApp(ui, server)
