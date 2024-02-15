library(shiny)
library(bs4Dash)
library(MASS)
library(ggplot2)
library(tidyr)
library(dplyr)

# general hint: remember to call and assign reactive values myvar <- reactiveVal(NULL) using parantheses () by myvar()
# this is not the case for reactiveValues()

source('drawing_functions.R')
source('classifier_calculations.R')

ui = dashboardPage(
  title = "Classifier Demo",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Data Settings",
      color = "primary",
      href = "https://scads.ai/",
      image = "https://scads.ai/wp-content/themes/scads/assets/images/scads_logo.svg"
    )
  ),
  sidebar = dashboardSidebar(
    minified = FALSE,
    tabsetPanel(
      tabPanel("Group 1", 
               sliderInput("n1", "Number of Observations:", value = 100, min = 10, max = 200, step = 5),
               fluidRow(
                 column(6, checkboxInput("equal_mean_1", label = "Equalize Mean", value = TRUE)),
                 column(6, checkboxInput("equal_sd_1", label = "Equalize SD", value = TRUE))
               ),
               sliderInput("mean1_1", "Mean of Variable 1:", value = -2, min = -10, max = 10),
               conditionalPanel(
                 condition = "input.equal_mean_1 == false",
                 sliderInput("mean2_1", "Mean of Variable 2:", value = -2, min = -10, max = 10)
               ),
               sliderInput("sd1_1", "Standard Deviation of Variable 1:", value = 1, min = 0.1, max = 5, step = 0.1),
               conditionalPanel(
                 condition = "input.equal_sd_1 == false",
                 sliderInput("sd2_1", "Standard Deviation of Variable 2:", value = 1, min = 0.1, max = 5, step = 0.1)
               ),
               sliderInput("cor_1", "Correlation coefficient:", min = -1, max = 1, value = 0, step = 0.1)
      ),
      tabPanel("Group 2",
               sliderInput("n2", "Number of Observations:", value = 100, min = 10, max = 200, step = 5),
               fluidRow(
                 column(6, checkboxInput("equal_mean_2", label = "Equalize Mean", value = TRUE)),
                 column(6, checkboxInput("equal_sd_2", label = "Equalize SD", value = TRUE))
               ),
               sliderInput("mean1_2", "Mean of Variable 1:", value = 0, min = -10, max = 10),
               conditionalPanel(
                 condition = "input.equal_mean_2 == false",
                 sliderInput("mean2_2", "Mean of Variable 2:", value = 0, min = -10, max = 10)
               ),
               sliderInput("sd1_2", "Standard Deviation of Variable 1:", value = 1, min = 0.1, max = 5, step = 0.1),
               conditionalPanel(
                 condition = "input.equal_sd_2 == false",
                 sliderInput("sd2_2", "Standard Deviation of Variable 2:", value = 1, min = 0.1, max = 5, step = 0.1)
               ),
               sliderInput("cor_2", "Correlation coefficient:", min = -1, max = 1, value = 0, step = 0.1)
      )
    ), 
    br(),
    checkboxInput("use_ggplot", value = TRUE, "Use ggplot")
  ),
  body = dashboardBody(
    box(title = 'Task description',
        width = 12,
        fluidRow(
          column(width = 3, "There are given two groups of people where something was observed. Please try to seperate these two groups as good as possible by drawing a straight line into the scatterplot.",),
          column(width = 3, 
                 align = "center", 
                 radioButtons("drawType", label = '',
                              choices = c("Draw linear function", "Draw freehand line"),
                              selected = "Draw linear function")
                 ),
          column(width = 6, align = "center", tableOutput("classificationTable"))
          )
        ),
    box(
      #title = "Scatterplot",
      width = 12,
      plotOutput("scatterPlot", 
                 height = "600px", 
                 click = "plot_click", 
                 hover = hoverOpts(
                   id = "plot_hover",
                   delay = 180,
                   delayType = "throttle")
      )
    ),
    box(
      title = 'Debugging area',
      width = 12,
      # Display mouse and boundary information
      verbatimTextOutput("mouse_position"), 
      fluidRow(
        column(width = 6, verbatimTextOutput("click_info")),
        column(width = 6, verbatimTextOutput("boundary_info"))
      ),
      collapsed = TRUE
    )
  )
  #controlbar = dashboardControlbar()
)


server = function(input, output, session) { 
  
  ######## Data related stuff #############
  DataParams <- reactive({
    group1_params <- list(
      n1 = input$n1,
      mean1 = input$mean1_1,
      mean2 = if(input$equal_mean_1) input$mean1_1 else input$mean2_1,
      sd1 = input$sd1_1,
      sd2 = if(input$equal_sd_1) input$sd1_1 else input$sd2_1,
      cor = input$cor_1)
    
    group2_params <- list(
      n = input$n2,
      mean1 = input$mean1_2,
      mean2 = if(input$equal_mean_2) input$mean1_2 else input$mean2_2,
      sd1 = input$sd1_2,
      sd2 = if(input$equal_sd_2) input$sd1_2 else input$sd2_2,
      cor = input$cor_2)
    
    list(group1_params = group1_params, group2_params = group2_params)
  })
 
  data <- reactive({
    generateData(DataParams()$group1_params, DataParams()$group2_params)
  })
  
  ########## Classifier stuff ###############
  # parameters of linear decision boundary
  decision_boundary_parameters <- reactiveValues(
    slope = NULL, 
    intercept = NULL
  )
  
  # reactive expression for the decision boundary
  decision_boundary <- reactive({
    if (!is.null(data()) && !is.null(decision_boundary_parameters)) {
      decision_boundary_parameters$intercept + decision_boundary_parameters$slope * data()$Variable1
    } else {
      NULL
    }
  })
  
  # Create data frame for classification results
  classification_results <- reactive({
    req(decision_boundary())
    data.frame(Group = data()$Group,
               Above_Below_Line = ifelse(data()$Variable2 > decision_boundary(), "Above", "Below")
               )
    })
  
  output$classificationTable <- renderTable({
    if (!is.null(data()) && !is.null(classification_results)) {
      # Summary table with aggregated results
      summary_table <- classification_results() %>%
        group_by(Group, Above_Below_Line) %>%
        summarise(Count = n()) %>%
        ungroup() %>%
        pivot_wider(names_from = Above_Below_Line, values_from = Count, values_fill = 0)
      
      return(summary_table)
    } else {
      return(NULL)
    }
  })

  ############## Plotting stuff #################
  output$scatterPlot <- renderPlot({
    # make sure that data() contains something
    req(data())
    
    # Plotting code using 'data'
    if (input$use_ggplot){
      plotScatterAndLine_ggplot(data(), clickedPoints, mouse_position, line_color = "red", line_width = 2)
    } else {
      plotScatterAndLine(inputdata = data(),
                         group = data()$Group,
                         clickedPoints = clickedPoints,
                         mouse_position = mouse_position,
                         line_width = 3,
                         line_color = "red")
    }
  })
  
  ######### Mouse related stuff ############
  # Reactive value to store mouse position
  mouse_position <- reactiveVal(NULL)
  
  clickedPoints <- reactiveValues(
    click_coords = matrix(0, ncol = 2, nrow = 0),
    line_exists = FALSE
  )
  
  observe({
    # Update mouse position when hovering
    if (!is.null(input$plot_hover)) {
      mouse_position(input$plot_hover)
    }
  })
  
  observeEvent(input$plot_click, {
    click_data <- input$plot_click
    
    # reset everything if existing line
    if (clickedPoints$line_exists == TRUE) {
      clickedPoints$click_coords <- matrix(0, ncol = 2, nrow = 0)
      clickedPoints$line_exists = FALSE
    }
    
    if (!is.null(click_data)) {
      clickedPoints$click_coords <- rbind(clickedPoints$click_coords, c(click_data$x, click_data$y))
      if (nrow(clickedPoints$click_coords)==2){
        result <- calculate_decision_boundary(clickedPoints)
        
        # If the result is not NULL, update the reactiveValues
        if (!is.null(result)) {
          decision_boundary_parameters$slope <- result$slope
          decision_boundary_parameters$intercept <- result$intercept
        }
      }
    }
  })
  
  ######### info and debugging stuff ##################
  
  output$click_info <- renderPrint({
    # Display click coordinates 
    cat('clicked coordinates: \n')
    clickedPoints$click_coords
  })
  
  output$boundary_info <- renderPrint({
    cat("Decision Boundary Parameters:\n")
    cat("Slope: ", decision_boundary_parameters$slope, "\n")
    cat("Intercept: ", decision_boundary_parameters$intercept)
  })
  
  output$mouse_position <- renderPrint({
    # Display the most recent mouse position
    if (!is.null(mouse_position())) {
      paste0("Mouse Position: (", mouse_position()$x,
             ", ", mouse_position()$y,
             "), num clicks: ", nrow(clickedPoints$click_coords),
             ", line_exists:", clickedPoints$line_exists)
    } else {
      'Not yet any mouse position on scatterplot.'
    }
  })
}

shinyApp(ui = ui, server = server)

