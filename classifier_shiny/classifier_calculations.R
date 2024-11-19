# classifier_calculations.R

generateData <- function(seed, group1_params, group2_params, debug_info = FALSE) {
  if (debug_info) {
    print(seed)
    print(group1_params)
    print(group2_params)
  }
  
  set.seed(seed)
  
  # Generate bivariate normal data for Group 1
  data1 <- mvrnorm(n = group1_params$n,
                   mu = c(group1_params$mean1, group1_params$mean2),
                   Sigma = matrix(c(group1_params$sd1^2, group1_params$cor * group1_params$sd1 * group1_params$sd2,
                                    group1_params$cor * group1_params$sd1 * group1_params$sd2, group1_params$sd2^2), nrow = 2),
                   empirical = FALSE)
  
  # Generate bivariate normal data for Group 2
  data2 <- mvrnorm(n = group2_params$n,
                   mu = c(group2_params$mean1, group2_params$mean2),
                   Sigma = matrix(c(group2_params$sd1^2, group2_params$cor * group2_params$sd1 * group2_params$sd2,
                                    group2_params$cor * group2_params$sd1 * group2_params$sd2, group2_params$sd2^2), nrow = 2),
                   empirical = FALSE)
  
  # Combine the data with group assignments
  group1 <- data.frame(Variable1 = data1[, 1], Variable2 = data1[, 2], Group = as.factor("Group1"))
  group2 <- data.frame(Variable1 = data2[, 1], Variable2 = data2[, 2], Group = as.factor("Group2"))
  
  return(rbind(group1, group2))
}

# Function to calculate decision boundary parameters from two points
calculate_linear_boundary <- function(input_coordinates) {
  #print(paste(input_coordinates$x, input_coordinates$y))
  # Ensure there are two clicked points
  if (length(input_coordinates$x) != 2) {
    return(NULL)
  }
  
  # Extract the coordinates of the two clicked points
  x1 <- input_coordinates$x[1]
  y1 <- input_coordinates$y[1]
  x2 <- input_coordinates$x[2]
  y2 <- input_coordinates$y[2]
  
  # Calculate slope and intercept
  slope <- (y2 - y1) / (x2 - x1)
  intercept <- y1 - slope * x1
  
  # Return the parameters as a list
  return(list(slope = slope, intercept = intercept))
}

is_above_linear <- function(params, compare_data){
  print(paste('linear params: ', params$intercept, params$slope))
  y_predicted <- params$intercept + params$slope*compare_data$Variable1
  above <- ifelse(compare_data$Variable2 > y_predicted, TRUE, FALSE)
  return(above)
}

is_above_freehandline <- function(x, y, compare_data, df_spline){
  # fitting spline
  smoothed <- smooth.spline(x, y, df = df_spline)
  
  # relate spline values for dedicated x-values to data y data points
  y_predicted <- predict(smoothed, compare_data$Variable1)$y
  above <- ifelse(compare_data$Variable2 > y_predicted, TRUE, FALSE)
  return(above)
}

calculate_classification_results <- function(referenceData, aboveData){
  tmp_df <- data.frame(Group = referenceData,
                       AboveBoundary = as.factor(aboveData)
  )
  tmp_df %<>% mutate(classification_group1_below = as.factor(ifelse(AboveBoundary==FALSE, 'Group1', 'Group2')), 
                     classification_group1_above = as.factor(ifelse(AboveBoundary==TRUE, 'Group1', 'Group2')))
  
  # initializing variable for storing the best confusion matrix (and therewith all connected metrics)
  best_cm <- NULL
  
  # Calculate confusion matrix for every case
  tmp_below <- tryCatch({
    confusionMatrix(data = tmp_df$classification_group1_below, 
                    reference = tmp_df$Group, 
                    positive = "Group1"
    )
  }, error = function(e) {
    NULL  # Return NULL to indicate failure
  })
  
  tmp_above <- tryCatch({
    confusionMatrix(data = tmp_df$classification_group1_above, 
                    reference = tmp_df$Group, 
                    positive = "Group1"
    )
  }, error = function(e) {
    NULL  # Return NULL to indicate failure
  })
  
  if (!is.null(tmp_below) && !is.na(tmp_below$byClass['F1'])) {
    if (tmp_below$byClass['F1']==1) {
      # check for perfect separation, directly returning this confusion matrix
      return(tmp_below)  
    } else {
      best_cm <- tmp_below
    }
  }
  
  if (!is.null(tmp_above) && !is.na(tmp_above$byClass['F1'])) {
    if (tmp_above$byClass['F1']==1) {
      # check for perfect separation, directly returning this confusion matrix
      return(tmp_above)  
    }
    if (!is.null(best_cm) && !is.na(best_cm)) {
      if (tmp_above$byClass['F1'] > best_cm$byClass['F1']) {
        return(tmp_above)
      } 
    } else {
      best_cm <- tmp_above
    }
      
  } 
  return(best_cm)
}

####### logistic regression #############
calculate_logistic_decision_boundary <- function(input_data) {
  # estimate logistic regression model
  model <- glm(as.factor(Group) ~ Variable1 + Variable2, family = binomial, data = input_data)
  
  # Coefficients of the logistic regression model
  coefficients <- coef(model)
  decision_boundary <- function(x1_val) {
    (-coefficients[1] - coefficients[2]*x1_val) / coefficients[3]
  }
  
  logistic_x1 <- range(input_data$Variable1)
  
  logistic_x2 <- decision_boundary(logistic_x1)
  tmp <- data.frame(x = logistic_x1, y = logistic_x2)
  # naming (x,y) is chosen according to function calculate_linear_boundary()
  return(calculate_linear_boundary(tmp))
}