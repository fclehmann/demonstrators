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
        color = "white",
        href = "https://scads.ai/",
        image = "https://scads.ai/wp-content/themes/scads2023/assets/images/logo.png"
      ),
      fixed = TRUE
  ),
  sidebar = dashboardSidebar(
    minified = FALSE, 
    width = "18%",
    # data area
    tabsetPanel(type = 'pills', 
                #selected = 'animal_classifier',
                id = 'main_tabs',
      tabPanel(title = "Stufe 1",
               radioButtons("predefinedsettings_selector", "Konfiguration wählen",
                            choices = names(predefined_settings),
                            selected = names(predefined_settings)[1]
                            ), 
               uiOutput("setting_description")
               ),
      #### animal classifier #####
      tabPanel(title = "Stufe 2",
               value = 'animal_classifier',
               textOutput(outputId = "animal_classifier_selection_message"), # Dynamic description, 
               div(style = "height: 10px;"),
               verbatimTextOutput(outputId = "animal_classifier_output"), # selection message for animal classifier categories
               uiOutput(outputId = "animal_classifier_checkboxes"),  # Display selected AnimalClassifierAvailCategories
               br(),
               sliderInput(inputId = "n_animal", label = "Anzahl Beobachtungen/Kategorie:", value = 20, min = 10, max = 40, step = 5),
               br(),
               checkboxInput(inputId = "animal_classifier_show_images", label = "Originalbild zeigen", value = TRUE)
      ),
      tabPanel(title = 'Stufe 3',
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
          style = "height: 150px; overflow: hidden; font-size: 14px;",
          column(width = 1, 
                 align = "left", 
                 radioButtons(inputId = "drawType", 
                              label = "Zeichenmodus",
                              choices = list("gerade Linie" = "line", "Freihandlinie" = "freehand"),
                              selected = "line"), 
                 div(style = "height: -10px;"),
                 actionButton(inputId = "reset", label = "Reset line")
          ),
          column(width = 2, align = 'left', textOutput("instructionsText")),
          ######################
          column(width = 2, 
                 align = "center",
                 conditionalPanel(
                   condition = "input.animal_classifier_show_images && input.main_tabs == 'animal_classifier'",
                   uiOutput(outputId = "animal_image_display")  # Placeholder for displaying animal classifier images
                 )
          ),
          #####################
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
        column(width = 2, align = "left", verbatimTextOutput(outputId = 'debugging')),
        column(width = 7, align = "left", DTOutput(outputId = "DataTable")),
        column(width = 2, align = "center", tableOutput(outputId = "AboveTable"))
      ),
    collapsed = TRUE
    )
  )
  #controlbar = dashboardControlbar()
)}

################ server function ########################
server <- function(input, output, session) {
  
  # Create a unique directory for each session using session token (or ID)
  tmp_parent <- "data/tmp-session/"
  user_dir <- file.path(tmp_parent, session$token)
  dir.create(path = user_dir, recursive = T, showWarnings = T)
  cat('tmp-dir: ', user_dir, '\n')
  
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
  
  ##### non-animal classifier configuration ####
  
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
  
  ##### animal classifier configuration ####
  
  if (file.exists('data/animal_classifier-last_layer_data.Rdata')){
    cat('loading animal classifier data file...\n')
    load(file = 'data/animal_classifier-last_layer_data.Rdata')
    cat('existing obs: ', ls(), '\n')
    animal_classifier_avail_categories <- animal_classifier_df$true_class %>% unique()  
    animal_classifier_df %<>% 
      dplyr::select(-predicted_class_idx, -output_3) %>% 
      mutate(Group = true_class)
    # this renaming is done that the structure is compatible with existing calculation and drawing functions
    animal_classifier_df %<>% rename(Variable1 = output_1, Variable2 = output_2)
    animal_classifier_df %<>% mutate(image_input = basename(image_input))
    animal_classifier_images <- list.files(path = 'www/', recursive = TRUE)
    animal_classifier_df %<>% mutate(image_input = animal_classifier_images[match(image_input, basename(animal_classifier_images))])
  }
  
  IsAnimalClassifier <- reactive({
    # check activation of animal classifier tab
    input$main_tabs == "animal_classifier"
  })
      
  AnimalClassifierConfig <- reactive({
    req(validated_seed())
    seed <- as.numeric(validated_seed())
    # Check if exactly two categories are selected
    selected_categories <- input$animal_classifier_selected_categories
    n_animal <- input$n_animal  # Assuming the slider is called `animal_classifier_slider`
    
    # If two categories are selected, return them as a list with the new 'n_animal' property
    if (length(selected_categories) == 2) {
      return(list(
        categories = selected_categories,  
        n_animal = n_animal, 
        seed = seed
      ))
    } else {
      # Return NULL if not exactly two categories are selected
      return(NULL)
    }
  })
  
  # Generate dynamic checkboxes for animal classifier
  output$animal_classifier_checkboxes <- renderUI({
    # Count the number of selected animal_classifier_categories
    selected_count <- length(input$animal_classifier_selected_categories)
    
    # Dynamically set the label based on selection count
    dynamic_label <- if (selected_count == 0) {
      "Zwei Kategorien auswählen..."
    } else if (selected_count == 1) {
      "Eine weitere Kategorie wählen..."
    } else {
      paste0("Kategorien gewählt.") 
    }
    
    # Render the checkboxGroupInput with the dynamic label
    checkboxGroupInput(
      inputId = "animal_classifier_selected_categories", 
      label = dynamic_label,  
      choices = animal_classifier_avail_categories,
      selected = if (is.null(input$animal_classifier_selected_categories)) {
        head(animal_classifier_avail_categories, 2)  # Default to first two categories
      } else {
        input$animal_classifier_selected_categories  # Preserve current selection
      }
    )
    
  })
  
  # Restrict the number of selected animal_classifier_categories to a maximum of two
  observeEvent(input$animal_classifier_selected_categories, {
    if (length(input$animal_classifier_selected_categories) > 2) {
      # Revert to the first two selected AnimalClassifierAvailCategories
      updateCheckboxGroupInput(
        session,
        inputId = "animal_classifier_selected_categories",
        selected = input$animal_classifier_selected_categories[1:2]
      )
    } 
  })

  #### general configuration ####
  
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
    if (!IsAnimalClassifier()) {
      return(generateData(DataParams()$seed, DataParams()$group1_params, DataParams()$group2_params))
    } else if (!is.null(AnimalClassifierConfig())) {
      # assign axis and group labels
      plot_labels$xlabel <- 'völlig unvorstellbare Dimension 1'
      plot_labels$ylabel <- 'völlig unvorstellbare Dimension 2'
      plot_labels$group1_label <- AnimalClassifierConfig()$categories[1]
      plot_labels$group2_label <- AnimalClassifierConfig()$categories[2]
      set.seed(AnimalClassifierConfig()$seed)
      animal_plot_data <- animal_classifier_df %>% 
        filter(Group %in% AnimalClassifierConfig()$categories) %>% 
        group_by(Group) %>% 
        sample_n(size = AnimalClassifierConfig()$n_animal, replace = FALSE)
      animal_plot_data %<>% mutate(Group = as.factor(if_else(Group == AnimalClassifierConfig()$categories[1], true = 'Group1', false = 'Group2')))
      return(animal_plot_data)
    }
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
  
  output$animal_image_display <- renderUI({
    if (!draw() && !is.null(data())) {
      hover <- input$plot_hover
      if (!is.null(hover)) {
        # Get the closest point by matching coordinates
        closest_point <- which.min(abs(data()$Variable1 - hover$x) + abs(data()$Variable2 - hover$y))
        image_file <- data()$image_input[closest_point]
        x_coord <- round(data()$Variable1[closest_point], 2)
        y_coord <- round(data()$Variable2[closest_point], 2)
        tagList(
          div(
            style = "height: 120px;",
            tags$img(src = image_file, style = "height: 95%; width: auto;"),
            tags$p(paste0('(x,y)=', '(', x_coord, ',', y_coord, ')'))
          )
        )
      } else {
        return(NULL)  
      }
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
  
  # Cleanup: delete temporary files when the session ends
  session$onSessionEnded(function() {
    if (dir.exists(user_dir)) {
      unlink(user_dir, recursive = TRUE)
      message("Temporary files deleted for session:", session$token)
    }
  })
  
  ################### debugging stuff #####################
  output$debugging <- renderPrint({
    #logisticBoundary()
    print(paste0('user defined boundary coordinates: ', coords$x, coords$y, ' logistic boundary parameters: ', logisticBoundary()))
    print(paste0('animal_classifier = ', IsAnimalClassifier()))
    print(paste0('available animal categories: ', animal_classifier_avail_categories))
    print(paste0('selected animal categories: ', AnimalClassifierConfig()$categories))
    print(paste0('animal n: ', AnimalClassifierConfig()$n_animal))
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
