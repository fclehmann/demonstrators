library(shiny)

ui <- fluidPage(
  h4("Click on plot to start drawing, click again to pause"),
  sliderInput("mywidth", "width of the pencil", min=1, max=30, step=1, value=10),
  sliderInput("degree", "Degree of polynomial:", min = 1, max = 100, value = 50),
  actionButton("reset", "reset"),
  fluidRow(
    column(width = 6, plotOutput("plot", width = "100%", height = "500px",
                                 hover=hoverOpts(id = "hover", delay = 50, delayType = "throttle", clip = TRUE, nullOutside = TRUE),
                                 click="click")),
    column(width = 6, plotOutput("spline_plot", width = "100%", height = "500px"))
  )
)

server <- function(input, output, session) {
  vals <- reactiveValues(x=NULL, y=NULL)
  draw <- reactiveVal(FALSE)
  
  observeEvent(input$click, handlerExpr = {
    temp <- draw()
    draw(!temp)
    
    if (!draw()) {
      vals$x <- c(vals$x, NA)
      vals$y <- c(vals$y, NA)
    }
  })
  
  observeEvent(input$reset, handlerExpr = {
    vals$x <- NULL
    vals$y <- NULL
    output$spline_plot <- renderPlot({}) # Reset spline plot
  })
  
  observeEvent(input$hover, {
    if (draw()) {
      x <- input$hover$x
      y <- input$hover$y
      
      if (is.null(vals$x) || x > tail(vals$x, 1)) {
        vals$x <- c(vals$x, x)
        vals$y <- c(vals$y, y)
      }
    }
  })
  
  output$plot <- renderPlot({
    plot(x=vals$x, y=vals$y, xlim=c(0, 28), ylim=c(0, 28), ylab="y", xlab="x", type="l", lwd=input$mywidth)
  })
  
  output$spline_plot <- renderPlot({
    if (!is.null(vals$x) && !is.null(vals$y)) {
      # Remove NA values
      x <- vals$x[!is.na(vals$x)]
      y <- vals$y[!is.na(vals$y)]
      
      # Fit polynomial spline
      spline_fit <- smooth.spline(x, y, df = input$degree)
      
      # Plot spline
      plot(x, y, type = "l", xlim = c(0, 28), ylim = c(0, 28), xlab = "x", ylab = "y")
      lines(spline_fit, col = "red")
    }
  })
}

shinyApp(ui, server)
