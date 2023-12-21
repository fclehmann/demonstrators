# ui.R

library(shiny)
library(DT)

shinyUI(fluidPage(
  titlePanel("Data Generator"),
  
  sidebarLayout(
    sidebarPanel(
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
      ),
      actionButton("generateDataBtn", "Generate Data"), 
      # Set the width of the sidebar
      style = "width: 200px;"
    ),
    
    mainPanel(
      fluidRow(
        column(width = 8,
               # Scatterplot
               plotOutput("scatterPlot", height = "600px")
        ),
        column(width = 4,
               # Sliders for intercept and slope
               h4("Decision Boundary Parameters"),
               sliderInput("slope", "Slope:", min = -5, max = 5, value = 1),
               sliderInput("intercept", "Intercept:", min = -10, max = 10, value = 0),
               tableOutput("classificationTable")
        )
      )
    )
  )
))
