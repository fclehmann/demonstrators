library(shiny)
library(bs4Dash)
library(dplyr)
library(magrittr)
library(ggplot2)
library(caret)
library(MASS)
library(log4r)

# Define logger configuration
logger <- logger()

source('classifier_calculations.R')
source('drawing_functions.R')

############ ui stuff ######################
ui = { 
  dashboardPage(
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
    # data area
    tabsetPanel(
      tabPanel(title = "Group 1", 
               sliderInput("n1", "Number of Observations:", value = 5, min = 10, max = 200, step = 5),
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
      tabPanel(title = "Group 2",
               sliderInput("n2", "Number of Observations:", value = 5, min = 10, max = 200, step = 5),
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
    checkboxInput(inputId = "use_rbaseplot", label = "Use R baseplot", value = TRUE)
  ),
  # core content 
  body = dashboardBody(
    box(title = 'Task description',
        width = 12,
        fluidRow(
          column(width = 2, "There are given two groups of people where something was observed. Please try to seperate these two groups as good as possible by drawing a straight line into the scatterplot.",),
          column(width = 2, 
                 align = "center", 
                 radioButtons(inputId = "drawType", 
                              label = "Drawing Mode:",
                              choices = list("Draw Line" = "line", "Draw Freehand" = "freehand"),
                              selected = "line"), 
                 br(),
                 actionButton(inputId = "reset", label = "Reset")
          ),
          column(width = 2, align = "right", tableOutput("AboveTable")), 
          column(width = 3, align = "center", verbatimTextOutput("classificationRule")),
          column(width = 3, align = "center", tableOutput("classificationTable"))
        )
    ),
    box(
      #title = "Scatterplot",
      width = 12,
      plotOutput("plot", 
                 height = "600px", 
                 click = "plot_click", 
                 hover = hoverOpts(
                   id = "plot_hover",
                   delay = 200,
                   delayType = "throttle")
      )
    ),
    box(
      title = 'Debugging area',
      width = 12,
      verbatimTextOutput('debugging'),
      collapsed = TRUE
    )
  )
  #controlbar = dashboardControlbar()
)}

################ server function ########################
server <- function(input, output, session) {
  
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
   
  ################ drawing logic ############################
  
  # draw() controls whether to collect hover coordinates or not
  draw <- reactiveVal(FALSE)
  drawing_finished <- reactiveVal(FALSE)
  coords <- reactiveValues(x=NULL, y=NULL)
  
  observeEvent(input$plot_click, handlerExpr = {
    if (!drawing_finished()){
      # hint: Clicking is needed in both drawing modes. Thus, logic is the same.
      draw(!draw())
      # indicate finished drawing
      if (draw()){
        # hint: We do not collect the click coordinates but the hover coordinates, as clicking slightly moves the mouse pointer which might lead to edges or bends at the esp. in case of line mode
        coords$x <- c(coords$x, input$plot_hover$x)
        coords$y <- c(coords$y, input$plot_hover$y)
      } else {
        # draw() is set FALSE by second click, thus drawing_finished() becomes TRUE (and triggers classification evaluation)
        drawing_finished(TRUE)
      }
    }
    info(logger, paste('draw(): ', draw(), ', ', input$drawType, 'finished: ', drawing_finished()))
  })
  
  observeEvent(input$plot_hover, {
    if (draw() && !drawing_finished()){
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
      }
      )
    }
  })
  
  ###################### classification stuff ##############################
  
  ######### old stuff  ########
  AboveData <- reactive({
    abovetmp <- NULL
    if (drawing_finished()){
      info(logger, paste('calculating above below data for ', input$drawType))
      switch(EXPR = input$drawType,
             freehand = {
               abovetmp <- is_above_freehandline(x = coords$x, y = coords$y, compare_data = data())
             },
             line = {
               linear_params <- calculate_linear_boundary(input_coordinates = coords)
               info(logger, linear_params)
               abovetmp <- is_above_linear(intercept = linear_params$intercept, slope = linear_params$slope, compare_data = data())
               info(logger, length(abovetmp))
             }
      )
    }
    return(abovetmp)
  })
  

  # Render table
  output$AboveTable <- renderTable({
    req(AboveData())
    # Count the occurrences of TRUE and FALSE
    counts <- table(AboveData())
    # Convert to data frame for rendering in table
    counts_df <- data.frame(isAbove = names(counts), Count = as.numeric(counts))
    return(counts_df)
  })
  
  output$classificationTable <- renderTable({
    req(AboveData())
    # Calculate confusion matrix 
    #print(AboveData())
    #tmp <- calculate_classification_results(referenceData = data()$Group, aboveData = AboveData())
    # summary_table <- classification_results() %>%
    #   group_by(Group, classification) %>%
    #   summarise(Count = n()) %>%
    #   ungroup() %>%
    #   pivot_wider(names_from = classification, values_from = Count, values_fill = 0)
    # 
    # # Modify column names
    # colnames(summary_table) <- c('true group', paste0("pred_", unique(classification_results()$classification)))
    
    # return(summary_table)
    return(NULL)
  })
  
  
  ###################### plotting stuff ####################################
  base_plot <- reactive({
    # hint: rbase plot does not allow for working layerwise as in ggplot, thus only for ggplot the base_plot is created
    if (!input$use_rbaseplot) {
      info(logger, 'creating base plot for ggplot...')
      return(plotScatter_ggplot(inputdata = data(), xlab = "footlength cm", ylab = "weight kg"))
    }
  })
  
  plotboundary_width <- 2
  plotboundary_color <- 'red'
  
  output$plot <- renderPlot({
    req(data())
    # info(logger, 'plot rendering')
    # test <- data() %>% colnames()
    # info(logger, test)
    if (input$use_rbaseplot) {
      # Draw the initial plot
      plotScatter_rbase(inputdata = data(), xlab = "footlength cm", ylab = "weight kg")
      if (!is.null(coords$x)) {
        lines(x=coords$x, y=coords$y, lwd=plotboundary_width, col = plotboundary_color)
      }
    } else {
      # ggplot version
      p <- base_plot()
      if (!is.null(coords$x)) {
        p <- base_plot() + geom_line(data = data.frame(x = coords$x, y = coords$y), aes(x = x, y = y), color = plotboundary_color, linewidth = plotboundary_width)
      } 
      p
    }
  })
  
  ################ reset stuff ###########################
  
  observeEvent(c(input$drawType, input$reset), handlerExpr = {
    info(logger, 'reset variables')
    coords$x <- NULL
    coords$y <- NULL
    draw(FALSE)
    drawing_finished(FALSE)
    
    # hint: use the <<- operator to assign NULL to the reactive expression outside its definition
    #AboveData <<- NULL
  })
  
}

shinyApp(ui, server)
