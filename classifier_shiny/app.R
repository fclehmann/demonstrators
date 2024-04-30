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
library(fresh)

# general hints: 
# 1. remember to call and assign reactiveVal, as e.g. myvar <- reactiveVal(NULL) using parantheses () by myvar()
# this is not the case for reactiveValues()

# 2. the req() function in Shiny treats a vector containing all FALSE values as non-existent. 
# This behavior stems from how req() evaluates its arguments.
# When you pass a vector of boolean values to req(), it evaluates whether any of the elements in the vector are TRUE. If all elements are FALSE, 
# req() interprets it as the condition not being met and treats it as non-existent.

# Define logger configuration
logger <- logger()

source('classifier_calculations.R')
source('drawing_functions.R')
source('predefined_settings.R')

############ ui stuff ######################
ui = { 
  dashboardPage(
    title = "Klassifikator Demo",
    dark = NULL, # deactivate dark/light theme switch
    help = NULL, # deactive help switch
    fullscreen = TRUE, # activate fullscreen switch
    header = dashboardHeader(
      title = dashboardBrand(
        title = "Klassifikator",
        color = "primary",
        href = "https://scads.ai/",
        image = "https://scads.ai/wp-content/themes/scads/assets/images/scads_logo.svg"
      ),
      fixed = TRUE
  ),
  sidebar = dashboardSidebar(
    minified = FALSE, 
    width = "18%",
    # data area
    tabsetPanel(type = 'pills', 
      tabPanel(title = "Einfacher Modus",
               radioButtons("predefinedsettings_selector", "Konfiguration wählen",
                            choices = names(predefined_settings),
                            selected = names(predefined_settings)[1]
                            ), 
               uiOutput("setting_description")
               ),
      tabPanel(title = 'Fortgeschritten',
               tabsetPanel(
      tabPanel(title = "Group A",
               sliderInput("n_1", "Number of Observations:", value = 10, min = 10, max = 200, step = 5),
               fluidRow(
                 column(6, checkboxInput("equal_mean_1", label = "Equalize Mean", value = FALSE)),
                 column(6, checkboxInput("equal_sd_1", label = "Equalize SD", value = FALSE))
               ),
               sliderInput("mean1_1", "Mean of Variable 1:", value = 15, min = 10, max = 40),
               conditionalPanel(
                 condition = "input.equal_mean_1 == false",
                 sliderInput("mean2_1", "Mean of Variable 2:", value = 40, min = 5, max = 200)
               ),
               sliderInput("sd1_1", "Standard Deviation of Variable 1:", value = 1.5, min = 1, max = 20, step = 0.1),
               conditionalPanel(
                 condition = "input.equal_sd_1 == false",
                 sliderInput("sd2_1", "Standard Deviation of Variable 2:", value = 8, min = 1, max = 20, step = 0.1),
               ),
               sliderInput("cor_1", "Correlation coefficient:", min = -1, max = 1, value = 0.5, step = 0.1)
      ),
      tabPanel(title = "Group B",
               sliderInput("n_2", "Number of Observations:", value = 10, min = 10, max = 200, step = 5),
               fluidRow(
                 column(6, checkboxInput("equal_mean_2", label = "Equalize Mean", value = FALSE)),
                 column(6, checkboxInput("equal_sd_2", label = "Equalize SD", value = FALSE))
               ),
               sliderInput("mean1_2", "Mean of Variable 1:", value = 25, min = 10, max = 40),
               conditionalPanel(
                 condition = "input.equal_mean_2 == false",
                 sliderInput("mean2_2", "Mean of Variable 2:", value = 80, min = 5, max = 200)
               ),
               sliderInput("sd1_2", "Standard Deviation of Variable 1:", value = 1.5, min = 1, max = 20, step = 0.1),
               conditionalPanel(
                 condition = "input.equal_sd_2 == false",
                 sliderInput("sd2_2", "Standard Deviation of Variable 2:", value = 9, min = 1, max = 20, step = 0.1),
               ),
               sliderInput("cor_2", "Correlation coefficient:", min = -1, max = 1, value = 0.5, step = 0.1)
      )
    ),
               )
    ),
    br(),
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
                                width = '40%'), # width refers to column width of the layout
                   checkboxInput(inputId = "show_logistic_boundary", label = "logistische Regression", value = FALSE)
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
        column(width = 3, align = "left", verbatimTextOutput(outputId = 'debugging')),
        column(width = 5, align = "left", DTOutput(outputId = "DataTable")),
        column(width = 2, align = "center", tableOutput(outputId = "AboveTable"))
      ),
    collapsed = TRUE
    )
  )
  #controlbar = dashboardControlbar()
)}

################ server function ########################
server <- function(input, output, session) {
  
  ######## Data related stuff #############
  # The reactivity of data generation is based on changes of the ui elements that control the data parameters.
  # Therefore, predefined settings are applied by changing the ui elements values'.
  # Changing the ui elements directly is possible as well.
  # The final changes are done in reactive DataParams().
  
  # set labels to default values, using the predefined settings can overwrite these
  
  xlabel_default <- 'Variable 1'
  ylabel_default <- 'Variable 2'
  group1_label_default = 'Group A'
  group2_label_default = 'Group B'
  plot_labels <- reactiveValues(xlabel = xlabel_default, ylabel = ylabel_default, 
                                group1_label = group1_label_default, group2_label = group2_label_default)
  
  # Observe changes in the predefined settings and update the slider values accordingly.
  observeEvent(input$predefinedsettings_selector, {
    # When a setting is selected, update UI elements based on predefined_settings
    selected_setting <- predefined_settings[[input$predefinedsettings_selector]]
    
    # identify the case of setting choice induced change
    
    if (!is.null(selected_setting)) {
      # Group A
      updateSliderInput(session, "n_1", value = selected_setting$n_1)
      updateSliderInput(session, "mean1_1", value = selected_setting$mean1_1)
      updateSliderInput(session, "mean2_1", value = selected_setting$mean2_1)
      updateSliderInput(session, "sd1_1", value = selected_setting$sd1_1)
      updateSliderInput(session, "sd2_1", value = selected_setting$sd2_1)
      updateSliderInput(session, "cor_1", value = selected_setting$cor_1)
      
      # Group B - assuming similar UI elements are defined for Group B
      updateSliderInput(session, "n_2", value = selected_setting$n_2)
      updateSliderInput(session, "mean1_2", value = selected_setting$mean1_2)
      updateSliderInput(session, "mean2_2", value = selected_setting$mean2_2)
      updateSliderInput(session, "sd1_2", value = selected_setting$sd1_2)
      updateSliderInput(session, "sd2_2", value = selected_setting$sd2_2)
      updateSliderInput(session, "cor_2", value = selected_setting$cor_2)
      
      # assign axis and group labels
      plot_labels$xlabel <- ifelse(!is.null(selected_setting$xlabel), selected_setting$xlabel, xlabel_default)
      plot_labels$ylabel <- ifelse(!is.null(selected_setting$ylabel), selected_setting$ylabel, ylabel_default)
      plot_labels$group1_label <- ifelse(!is.null(selected_setting$group1_label), selected_setting$group1_label, group1_label_default)
      plot_labels$group2_label <- ifelse(!is.null(selected_setting$group2_label), selected_setting$group2_label, group2_label_default)
    }
  })
  
  output$setting_description <- renderUI({
    tmp <- predefined_settings[[input$predefinedsettings_selector]]$description
    HTML(tmp, collapse = "<br/>")
  })
  
  # validate seed value because numericInput cannot be locked against manual manipulation
  validated_seed <- reactive({
    req(input$seed)  # Ensure input$seed is not NULL
    
    # Check if input$seed is a positive integer
    if (is.integer(input$seed) && input$seed > 0) {
      return(input$seed)
    } else {
      # If input$seed is not a positive integer, show a warning message
      showModal(
        modalDialog(
          title = "Invalid Input",
          "Please enter a positive integer for the seed value.",
          easyClose = TRUE
        )
      )
      return(NULL)
    }
  })
  
  DataParams <- reactive({
    req(validated_seed())
    seed <- as.numeric(validated_seed())

    group1_params <- list(
      n = input$n_1,
      mean1 = input$mean1_1,
      mean2 = if(input$equal_mean_1) input$mean1_1 else input$mean2_1,
      sd1 = input$sd1_1,
      sd2 = if(input$equal_sd_1) input$sd1_1 else input$sd2_1,
      cor = input$cor_1)

    group2_params <- list(
      n = input$n_2,
      mean1 = input$mean1_2,
      mean2 = if(input$equal_mean_2) input$mean1_2 else input$mean2_2,
      sd1 = input$sd1_2,
      sd2 = if(input$equal_sd_2) input$sd1_2 else input$sd2_2,
      cor = input$cor_2)

    list(seed = seed, group1_params = group1_params, group2_params = group2_params)
  })
  
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
  
  logisticBoundary <- reactive({
    tmp <- NULL
    req(data())
    info(logger, 'calculate logistic regression... ')
    tmp <- calculate_logistic_decision_boundary(input_data = data())
    return(tmp)
  })
  
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
    classification_data %<>%
      mutate(Reference = case_when(
        Reference == "Group1" ~ plot_labels$group1_label,
        Reference == "Group2" ~ plot_labels$group2_label,
        TRUE ~ as.character(Reference)  # Keep the original value if it doesn't match any condition
      ))
    
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
      return(plotScatter_ggplot(inputdata = data(), xlab = plot_labels$xlabel, ylab = plot_labels$ylabel, 
                                group1_label = plot_labels$group1_label, group2_label = plot_labels$group2_label))
    }
  })
  
  plotboundary_width <- 2
  plotboundary_color <- 'red'
  plotboundary_logreg_color <- 'gray'
  plotboundary_logreg_linetype <- 'dashed'
  plotboundary_logreg_linetype_rbase <- 2
  
  output$plot <- renderPlot({
    req(data())
    # Get the limits of x and y axes from the base plot
    x_limits <- c(min(data()$Variable1), max(data()$Variable1))
    y_limits <- c(min(data()$Variable2), max(data()$Variable2))
    
    # hint: note the inconsistency when working with reactive() vs. reactiveValue, e.g. logisticBoundary()$x1 vs. coords$x
    if (input$use_rbaseplot) {
      # Draw the initial plot
      plotScatter_rbase(inputdata = data(), 
                        xlab = plot_labels$xlabel, ylab = plot_labels$ylabel, 
                        group1_label = plot_labels$group1_label, group2_label = plot_labels$group2_label)
      if (!is.null(coords$x)) {
        lines(x=coords$x, y=coords$y, lwd=plotboundary_width, col = plotboundary_color)
      }
      if (!is.null(logisticBoundary) && input$show_logistic_boundary) {
        abline(a = logisticBoundary()$intercept, b = logisticBoundary()$slope, 
              lwd=plotboundary_width, 
              col = plotboundary_logreg_color, 
              lty = plotboundary_logreg_linetype_rbase)
      }
    } else {
      # ggplot version
      p <- base_plot()
      if (!is.null(coords$x)) {
        p <- base_plot() + geom_line(data = data.frame(x = coords$x, y = coords$y), aes(x = x, y = y), 
                                     color = plotboundary_color, 
                                     linewidth = plotboundary_width)
      } 
      if (!is.null(logisticBoundary) && input$show_logistic_boundary) {
        p <- p + geom_abline(intercept = logisticBoundary()$intercept, slope = logisticBoundary()$slope, 
                           color = plotboundary_logreg_color, 
                           linewidth = plotboundary_width, 
                           linetype = plotboundary_logreg_linetype)
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
    updateCheckboxInput(session, "show_logistic_boundary", value = FALSE)
    # hint: use the <<- operator to assign NULL to the reactive expression outside its definition
    #AboveData <<- NULL
  })
  
  ################### debugging stuff #####################
  output$debugging <- renderPrint({
    #logisticBoundary()
    print(paste('user defined boundary coordinates: ', coords$x, coords$y, ' logistic boundary parameters: ', logisticBoundary()))
  })
  
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
