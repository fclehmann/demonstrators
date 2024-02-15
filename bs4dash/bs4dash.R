library(shiny)
library(bs4Dash)

ui <- dashboardPage(
  header = dashboardHeader(title = "Data Generator -hello world"),
  sidebar = dashboardSidebar(
    tabsetPanel(
      tabPanel("Group 1", 
               sliderInput("n1", "Number of Observations:", value = 100, min = 10, max = 200, step = 5),
               sliderInput("mean_1", "Mean of Variables:", value = 0, min = -10, max = 10),
               sliderInput("sd_1", "Standard Deviation:", value = 1, min = 0.1, max = 5, step = 0.1),
               sliderInput("cor_1", "Correlation coefficient:", min = -1, max = 1, value = 0, step = 0.1)
      ),
      tabPanel("Group 2",
               sliderInput("n2", "Number of Observations:", value = 100, min = 10, max = 200, step = 5),
               sliderInput("mean_2", "Mean of Variables:", value = 0, min = -10, max = 10),
               sliderInput("sd_2", "Standard Deviation:", value = 1, min = 0.1, max = 5, step = 0.1),
               sliderInput("cor_2", "Correlation coefficient:", min = -1, max = 1, value = 0, step = 0.1)
      )
    )
  ),
  body = dashboardBody(
    fluidRow(
      box(
        title = "Plot",
        width = 12,
        plotOutput("scatterPlot", height = "600px", click = "plot_click", hover = "plot_hover")
      )
    )
  ),
  controlbar = dashboardControlbar(
    controlSidebar(
      width = "50%"  # Set the width of the sidebar when expanded to half the screen size
    )
  ),
  options = list(
    collapsed = TRUE  # Collapse the sidebar by default
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
