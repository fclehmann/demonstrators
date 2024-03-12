library(shiny)
library(bs4Dash)
library(DT)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(caret)
library(MASS)
library(log4r)

# general hints: 
# 1. remember to call and assign reactive values myvar <- reactiveVal(NULL) using parantheses () by myvar()
# this is not the case for reactiveValues()

# 2. the req() function in Shiny treats a vector containing all FALSE values as non-existent. 
# This behavior stems from how req() evaluates its arguments.
# When you pass a vector of boolean values to req(), it evaluates whether any of the elements in the vector are TRUE. If all elements are FALSE, 
# req() interprets it as the condition not being met and treats it as non-existent.

# reflecting real data of anthropometric data (estimated on approx. 6000 observations (2000 female, 4000 male)): 
# Gender mean_ft mean_kg sd_ft sd_kg  corr
# Female    24.6    67.8  1.24  11.0 0.536
# Male      27.1    85.5  1.31  14.2 0.484


# Define logger configuration
logger <- logger()

source('classifier_calculations.R')
source('drawing_functions.R')
source('predefined_settings.R')

############ ui stuff ######################
ui = { 
  dashboardPage(
  title = "Klassifikator Demo",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Klassifikator",
      color = "primary",
      href = "https://scads.ai/",
      image = "https://scads.ai/wp-content/themes/scads/assets/images/scads_logo.svg"
    )
  ),
  sidebar = dashboardSidebar(
    minified = FALSE,
    # data area
    # tabsetPanel(
    #   tabPanel(title = "Group 1",
    #            sliderInput("n1", "Number of Observations:", value = 10, min = 10, max = 200, step = 5),
    #            fluidRow(
    #              column(6, checkboxInput("equal_mean_1", label = "Equalize Mean", value = FALSE)),
    #              column(6, checkboxInput("equal_sd_1", label = "Equalize SD", value = FALSE))
    #            ),
    #            sliderInput("mean1_1", "Mean of Variable 1:", value = 15, min = 10, max = 40),
    #            conditionalPanel(
    #              condition = "input.equal_mean_1 == false",
    #              sliderInput("mean2_1", "Mean of Variable 2:", value = 40, min = 5, max = 200)
    #            ),
    #            sliderInput("sd1_1", "Standard Deviation of Variable 1:", value = 1.5, min = 1, max = 20, step = 0.1),
    #            conditionalPanel(
    #              condition = "input.equal_sd_1 == false",
    #              sliderInput("sd2_1", "Standard Deviation of Variable 2:", value = 8, min = 1, max = 20, step = 0.1),
    #            ),
    #            sliderInput("cor_1", "Correlation coefficient:", min = -1, max = 1, value = 0.5, step = 0.1)
    #   ),
    #   tabPanel(title = "Group 2",
    #            sliderInput("n2", "Number of Observations:", value = 10, min = 10, max = 200, step = 5),
    #            fluidRow(
    #              column(6, checkboxInput("equal_mean_2", label = "Equalize Mean", value = FALSE)),
    #              column(6, checkboxInput("equal_sd_2", label = "Equalize SD", value = FALSE))
    #            ),
    #            sliderInput("mean1_2", "Mean of Variable 1:", value = 25, min = 10, max = 40),
    #            conditionalPanel(
    #              condition = "input.equal_mean_2 == false",
    #              sliderInput("mean2_2", "Mean of Variable 2:", value = 80, min = 5, max = 200)
    #            ),
    #            sliderInput("sd1_2", "Standard Deviation of Variable 1:", value = 1.5, min = 1, max = 20, step = 0.1),
    #            conditionalPanel(
    #              condition = "input.equal_sd_2 == false",
    #              sliderInput("sd2_2", "Standard Deviation of Variable 2:", value = 9, min = 1, max = 20, step = 0.1),
    #            ),
    #            sliderInput("cor_2", "Correlation coefficient:", min = -1, max = 1, value = 0.5, step = 0.1)
    #   )
    # ), 
    radioButtons("predefinedsettings", "Konfiguration wählen",
                 choices = names(predefined_settings),
                 selected = NULL),
    br(),
    div(style = "height: 400px;"),
    checkboxInput(inputId = "use_rbaseplot", label = "Grafik beschleunigen", value = FALSE)
  ),
  # core content 
  body = dashboardBody(
    box(title = 'Übersicht und Auswertung',
        width = 12,
        fluidRow(
          column(width = 2, 
                 align = "left", 
                 radioButtons(inputId = "drawType", 
                              label = "Zeichenmodus",
                              choices = list("gerade Linie" = "line", "Freihandlinie" = "freehand"),
                              selected = "line"), 
                 div(style = "height: -10px;"),
                 actionButton(inputId = "reset", label = "Reset line")
          ),
          column(width = 2, align = 'left', textOutput("instructionsText")),
          #column(width = 2, align = 'left', style = "white-space: normal;", verbatimTextOutput("instructionsText")),
          column(width = 5, align = "center", tableOutput("classificationTable")), 
          column(width = 2, 
                 align = "left",
                 conditionalPanel(
                   condition = "output.classificationTableExists",
                   'Hier Datensatz auswählen...', 
                   br(),
                   numericInput(inputId = "seed", 
                                label = "Datensatz Nr.", 
                                value = 1, min = 1, max = 30000, step = 1, 
                                width = '40%') # width refers to column width of the layout
                 )
          )
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
      fluidRow(
        column(width = 2, align = "left", verbatimTextOutput('debugging')),
        column(width = 4, align = "left", DTOutput("DataTable")),
        column(width = 2, align = "center", tableOutput("AboveTable"))
      ),
      collapsed = TRUE
    )
  )
  #controlbar = dashboardControlbar()
)}

################ server function ########################
server <- function(input, output, session) {
  
  ######## Data related stuff #############
  DataParams <- reactive({

    seed <- as.numeric(input$seed)

    group1_params <- list(
      n1 = predefined_settings[[input$predefinedsettings]]$n1,
      mean1 = predefined_settings[[input$predefinedsettings]]$mean1_1,
      mean2 = predefined_settings[[input$predefinedsettings]]$mean2_1,
      sd1 = predefined_settings[[input$predefinedsettings]]$sd1_1,
      sd2 = predefined_settings[[input$predefinedsettings]]$sd2_1,
      cor = predefined_settings[[input$predefinedsettings]]$cor_1)

    group2_params <- list(
      n = predefined_settings[[input$predefinedsettings]]$n2,
      mean1 = predefined_settings[[input$predefinedsettings]]$mean1_2,
      mean2 = predefined_settings[[input$predefinedsettings]]$mean2_2,
      sd1 = predefined_settings[[input$predefinedsettings]]$sd1_2,
      sd2 = predefined_settings[[input$predefinedsettings]]$sd2_2,
      cor = predefined_settings[[input$predefinedsettings]]$cor_2)

    list(seed = seed, group1_params = group1_params, group2_params = group2_params)
  })
  
  # DataParams <- reactive({
  # 
  #   seed <- as.numeric(input$seed)
  # 
  #   group1_params <- list(
  #     n1 = input$n1,
  #     mean1 = input$mean1_1,
  #     mean2 = if(input$equal_mean_1) input$mean1_1 else input$mean2_1,
  #     sd1 = input$sd1_1,
  #     sd2 = if(input$equal_sd_1) input$sd1_1 else input$sd2_1,
  #     cor = input$cor_1)
  # 
  #   group2_params <- list(
  #     n = input$n2,
  #     mean1 = input$mean1_2,
  #     mean2 = if(input$equal_mean_2) input$mean1_2 else input$mean2_2,
  #     sd1 = input$sd1_2,
  #     sd2 = if(input$equal_sd_2) input$sd1_2 else input$sd2_2,
  #     cor = input$cor_2)
  # 
  #   list(seed = seed, group1_params = group1_params, group2_params = group2_params)
  # })
  
  data <- reactive({
    generateData(DataParams()$seed, DataParams()$group1_params, DataParams()$group2_params)
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
  
  output$instructionsText <- renderText({
    text <- 'NULL'
    if (!draw() && !drawing_finished()) {
      text <- switch (EXPR = input$drawType,
                      freehand = 'Freihandlinie von links beginnend zeichnen. Mausklick in Grafikbereich, um Startpunkt der Linie zu setzen.',
                      line = 'Mausklick in Grafikbereich, um Startpunkt der Linie zu setzen.'
      )
    } else {
      if (draw() && !drawing_finished()) {
        text <- switch (EXPR = input$drawType,
                freehand = 'Mauszeiger entlang des Pfades der gewünschten Freihandlinie bewegen und anschließend klicken, um Endpunkt der Linie zu setzen.',
                line = 'Mauszeiger zum Ende der gewünschten Linie bewegen und anschließend klicken, um Endpunkt der Linie zu setzen.'
        )
      } else {
        text <- 'Jetzt kann die Klassifikationsgrenze auf verschiedene Datensätze angewendet werden. Für neu zeichnen Button "Reset line" drücken oder anderen Zeichenmodus wählen.'
      }
    }
    HTML(text)
  })
  
  ###################### classification stuff ##############################
  
  AboveData <- reactive({
    abovetmp <- NULL
    if (drawing_finished()){
      info(logger, paste('calculating above below data for ', input$drawType))
      switch(EXPR = input$drawType,
             freehand = {
               abovetmp <- is_above_freehandline(x = coords$x, y = coords$y, compare_data = data())
             },
             line = {
               #coords_tmp <- coords()
               print(coords)
               linear_params <- calculate_linear_boundary(input_coordinates = coords)
               info(logger, paste('linear params: ', linear_params))
               abovetmp <- is_above_linear(params = linear_params, compare_data = data())
               info(logger, length(abovetmp))
             }
      )
    }
    return(abovetmp)
  })
  
  # this boolean is needed for technical reasons, as directly checking AboveData() not working
  # see hints at top of the file wrt behavior of shiny::req()
  AboveDataExists <- reactive({
    ifelse(!is.null(AboveData()), TRUE, FALSE)
  })
  
  output$classificationTable <- renderTable({
    req(AboveDataExists())
    # Calculate confusion matrix 
    print(AboveData())
    tmp <- calculate_classification_results(referenceData = data()$Group, aboveData = AboveData())
    print(tmp$table)
    # note that classification metrics are available in the tmp list and are not yet used
    
    classification_data <- tmp$table %>% as.data.frame() %>% 
      # change pivot_wider to transpose the final confusion matrix 
      # names_from = Reference or names_from Prediction
      pivot_wider(names_from = Prediction, values_from = Freq, values_fill = 0)
    # change the column to be sorted according to pivot_wider
    group_labels <- as.character(sort(unique(classification_data$Reference), decreasing = FALSE))
  
    # Modify column names
    colnames(classification_data) <- c('tatsächliche<br>Gruppe', 
                                        paste0("klassifiziert als <br>", group_labels))
    return(classification_data)
  }, bordered = TRUE, sanitize.text.function=identity) # sanitize text is needed to realize line breaks in html
  
  # this is needed for the conditional display of the button for generating new data
  # note that drawing_finished is used as indication that the classification table exists although is might not be the best way
  output$classificationTableExists <- reactive({
    if (!is.null(drawing_finished())){
      drawing_finished()
    } else {
      return(NULL)
    }
  })
  
  # making the reactive output$classificationTableExists accessible outside the server function as e.g. ui condition
  outputOptions(output, "classificationTableExists", suspendWhenHidden = FALSE)
  
  ###################### plotting stuff ####################################
  base_plot <- reactive({
    # hint: rbase plot does not allow for working layerwise as in ggplot, thus only for ggplot the base_plot is created
    if (!input$use_rbaseplot) {
      info(logger, 'creating base plot for ggplot...')
      return(plotScatter_ggplot(inputdata = data(), xlab = "Fußlänge in cm", ylab = "Gewicht in kg"))
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
      plotScatter_rbase(inputdata = data(), xlab = "Fußlänge in cm", ylab = "Gewicht in kg")
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
  
  ################### debugging stuff #####################
  output$DataTable <- renderDT({
    # DOM elements: the length menu (l), the search box (f), the table (t), 
    # the information summary (i), and the pagination control (p), processing indicator (r)
    # e.g. dom = 'tp' is table and pagination
    # see docs at https://datatables.net/reference/option/dom
    if (!is.null(AboveData())) {
      tmp_data <- cbind(data(), AboveData())
    } else {
      tmp_data <- data()
    }
    datatable(tmp_data, options = list(paging = TRUE, pageLength = 10, dom = 'tp'))
  })
  
  output$AboveTable <- renderTable({
    req(AboveDataExists())
    # Count the occurrences of TRUE and FALSE
    counts <- table(AboveData())
    # Convert to data frame for rendering in table
    #counts_df <- data.frame(isAbove = names(counts), Count = as.numeric(counts))
    return(counts)
  })
  
  
}

shinyApp(ui, server)


##### old DataParams() reactive ##############
# DataParams <- reactive({
#   
#   seed <- as.numeric(input$seed)
#   
#   group1_params <- list(
#     n1 = input$n1,
#     mean1 = input$mean1_1,
#     mean2 = if(input$equal_mean_1) input$mean1_1 else input$mean2_1,
#     sd1 = input$sd1_1,
#     sd2 = if(input$equal_sd_1) input$sd1_1 else input$sd2_1,
#     cor = input$cor_1)
#   
#   group2_params <- list(
#     n = input$n2,
#     mean1 = input$mean1_2,
#     mean2 = if(input$equal_mean_2) input$mean1_2 else input$mean2_2,
#     sd1 = input$sd1_2,
#     sd2 = if(input$equal_sd_2) input$sd1_2 else input$sd2_2,
#     cor = input$cor_2)
#   
#   list(seed = seed, group1_params = group1_params, group2_params = group2_params)
# })