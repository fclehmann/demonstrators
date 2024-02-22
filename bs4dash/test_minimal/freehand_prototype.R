library(shiny)

# Zeichenlogik: 
# einheitlich: 2 klicks begrenzen die Zeichenaktivität
# abweichung: 
#   - gespeicherte koordinaten
#   - gezeichnetes Objekt (Punkte vs. Linie)

# Load the dataset
data(mtcars)


ui <- fluidPage(
  h4("Click on plot to start draw_freehanding, click again to pause"),
  actionButton(inputId = "reset", label = "Reset"),
  radioButtons(inputId = "drawType", 
               label = "Drawing Mode:",
               choices = list("Draw Line" = "line", "Draw Freehand" = "freehand"),
               selected = "line"),
  checkboxInput(inputId = "use_rbaseplot", label = "Use R baseplot", value = FALSE),
  fluidRow(
    column(width = 6, plotOutput("plot", width = "100%", height = "500px",
                                 hover=hoverOpts(id = "plot_hover", delay = 180, delayType = "throttle", clip = TRUE, nullOutside = TRUE),
                                 click="plot_click"))
  )
)

server <- function(input, output, session) {
  
  # TODO drawing_finished <- reactiveVal(FALSE)
  
  coords <- reactiveValues(x=NULL, y=NULL)
  # draw() simply controls whether to collect hover coordinates or not
  draw <- reactiveVal(FALSE)
  
  observeEvent(input$plot_click, handlerExpr = {
    # hint: Click coordinates are needed in both drawing modes. Thus, logic is the same.
    coords$x <- c(coords$x, input$plot_click$x)
    coords$y <- c(coords$y, input$plot_click$y)
    draw(!draw())
    print(paste(draw(), input$drawType))
  })
  
  observeEvent(c(input$drawType, input$reset), handlerExpr = {
    coords$x <- NULL
    coords$y <- NULL
    draw(FALSE)
  })
  
  observeEvent(input$plot_hover, {
    if (draw()==TRUE){
      x <- input$plot_hover$x
      y <- input$plot_hover$y
      
      switch(EXPR = input$drawType,
      freehand = {
        # prevent user from drawing backwards
        # TODO: identify drawing direction
        if (is.null(coords$x) || x > tail(coords$x, 1)) {
          coords$x <- c(coords$x, x)
          coords$y <- c(coords$y, y)
        }  
      }, 
      line = {
        coords$x[2] <- x
        coords$y[2] <- y
        #print(paste(input$drawType, coords$x, coords$y))
      }
      )
    }
  })
  
  output$plot <- renderPlot({
    if (input$use_rbaseplot) {
      # Draw the initial plot
      plot(mtcars$hp, mtcars$mpg, xlab = "Horsepower", ylab = "Miles per Gallon", lwd=2)
      
      if (!is.null(coords$x)) {
        lines(x=coords$x, y=coords$y, lwd=2)  
      }
    } else {
      # Create ggplot object
      p <- ggplot(data = mtcars, aes(x = hp, y = mpg)) +
        geom_point() +
        labs(x = "Horsepower", y = "Miles per Gallon") +
        theme_bw()
      
      # Add lines based on the drawing mode
      if (!is.null(coords$x)) {
        p <- p + geom_line(data = data.frame(x = coords$x, y = coords$y), aes(x = x, y = y), size = 2)
      }
      
      # Print the plot
      p
    }
  })
}

shinyApp(ui, server)
