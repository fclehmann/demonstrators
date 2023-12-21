# server.R

library(shiny)
library(ggplot2)
library(DT)

shinyServer(function(input, output) {
  
  # Initialize reactiveValues for data and trigger
  data_and_trigger <- reactiveValues(all_data = NULL, generate_data = FALSE)
  
  # Create a reactive expression for the decision boundary
  decision_boundary <- reactive({
    input$intercept + input$slope * data_and_trigger$all_data$Variable1
  })
  
  # Generate data when the button is clicked
  observeEvent(input$generateDataBtn, {
    # Generate bivariate normal data for Group 1
    data1 <- MASS::mvrnorm(n = input$n1,
                           mu = c(input$mean_1, input$mean_1),
                           Sigma = matrix(c(input$sd_1^2, input$cor_1 * input$sd_1 * input$sd_1,
                                            input$cor_1 * input$sd_1 * input$sd_1, input$sd_1^2), nrow = 2),
                           empirical = TRUE)
    
    # Generate bivariate normal data for Group 2
    data2 <- MASS::mvrnorm(n = input$n2,
                           mu = c(input$mean_2, input$mean_2),
                           Sigma = matrix(c(input$sd_2^2, input$cor_2 * input$sd_2 * input$sd_2,
                                            input$cor_2 * input$sd_2 * input$sd_2, input$sd_2^2), nrow = 2),
                           empirical = TRUE)
    
    # Combine the data with group assignments
    group1 <- data.frame(Variable1 = data1[, 1], Variable2 = data1[, 2], Group = "Group 1")
    group2 <- data.frame(Variable1 = data2[, 1], Variable2 = data2[, 2], Group = "Group 2")
    data_and_trigger$all_data <- rbind(group1, group2)
    
    # Set the trigger to TRUE
    data_and_trigger$generate_data <- TRUE
    
  })
  
  output$classificationTable <- renderDT({
    if (!is.null(data_and_trigger$all_data)) {
      # Calculate decision boundary
      boundary <- decision_boundary()
      
      # Create a data frame for detailed classification results
      classification_results <- data.frame(
        True_Group = data_and_trigger$all_data$Group,
        Above_Below_Line = ifelse(data_and_trigger$all_data$Variable2 > boundary, "Above", "Below")
      )
      
      return(DT::datatable(classification_results))
    } else {
      return(NULL)
    }
  })
  
  
  # Display a scatterplot with user-defined decision boundary
  output$scatterPlot <- renderPlot({
    # Check if data needs to be generated
    if (data_and_trigger$generate_data) {
      # Reset the trigger
      data_and_trigger$generate_data <- FALSE
    }
    
    # Check if all_data exists before attempting to access its variables
    if (!is.null(data_and_trigger$all_data)) {
      # Calculate decision boundary
      boundary <- decision_boundary()
      
      # Calculate accuracy rates based on the decision boundary for each group
      predictions <- ifelse(data_and_trigger$all_data$Variable2 > boundary, "Group 2", "Group 1")
      accuracy_group1 <- sum(predictions[data_and_trigger$all_data$Group == "Group 1"] == "Group 1") / sum(data_and_trigger$all_data$Group == "Group 1") * 100
      accuracy_group2 <- sum(predictions[data_and_trigger$all_data$Group == "Group 2"] == "Group 2") / sum(data_and_trigger$all_data$Group == "Group 2") * 100
      
      # Display accuracy rates in the UI
      output$accuracyGroup1 <- renderText({
        paste("Accuracy Rate (Group 1):", round(accuracy_group1, 2), "%")
      })
      
      output$accuracyGroup2 <- renderText({
        paste("Accuracy Rate (Group 2):", round(accuracy_group2, 2), "%")
      })
      
      # Plot scatterplot using aes_string
      ggplot(data_and_trigger$all_data, aes_string(x = "Variable1", y = "Variable2", color = "Group")) +
        geom_point() +
        geom_abline(intercept = input$intercept, slope = input$slope, linetype = "dashed", color = "red") +
        theme_minimal() +
        coord_fixed(ratio = 0.6) +  # Set aspect ratio to 1:1
        scale_color_manual(values = c("orange", "blue"))  # Set colors for the groups
    } else {
      # If all_data is not yet generated, return an empty plot
      ggplot() +
        theme_void()
    }
  })
})
