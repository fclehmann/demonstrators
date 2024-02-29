# classifier_calculations.R

generateData <- function(group1_params, group2_params) {
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
  
  # Calculate F1 score for classification_group1_below
  tmp_below <- confusionMatrix(
    tmp_df$Group,
    tmp_df$classification_group1_below,
    positive = "Group1"
  )
  
  # Calculate F1 score for classification_group1_above
  tmp_above <- confusionMatrix(
    tmp_df$Group,
    tmp_df$classification_group1_above,
    positive = "Group1"
  )
  
  print(tmp_below$byClass['F1'])
  print(tmp_above$byClass['F1'])
  # Check for perfect separation
  perfect_separation <- is.nan(tmp_below$byClass['F1']) || is.nan(tmp_above$byClass['F1'])
  print(colnames(tmp_df))
  # Determine which classification to choose based on F1 scores
  if (perfect_separation) {
    # Handle perfect separation case
    # For example, choose the classification with the most observations
    if (sum(tmp_df$Group == "Group1") > sum(tmp_df$Group == "Group2")) {
      tmp_df %<>%
        dplyr::select(-classification_group1_above) %>%
        rename(classification = classification_group1_below)
    } else {
      tmp_df %<>%
        dplyr::select(-classification_group1_below) %>%
        rename(classification = classification_group1_above)
    }
  } else {
    # Regular case: choose based on F1 scores
    if (tmp_below$byClass['F1'] >= tmp_above$byClass['F1']) {
      tmp_df %<>%
        dplyr::select(-classification_group1_above) %>%
        rename(classification = classification_group1_below)
    } else {
      tmp_df %<>%
        dplyr::select(-classification_group1_below) %>%
        rename(classification = classification_group1_above)
    }
  }
  # print(tmp_df)
  return(tmp_df)
}