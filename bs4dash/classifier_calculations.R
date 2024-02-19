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
calculate_decision_boundary <- function(clickedPoints) {
  # Ensure there are two clicked points
  if (nrow(clickedPoints$click_coords) != 2) {
    return(NULL)
  }
  
  # Extract the coordinates of the two clicked points
  x1 <- clickedPoints$click_coords[1, 1]
  y1 <- clickedPoints$click_coords[1, 2]
  x2 <- clickedPoints$click_coords[2, 1]
  y2 <- clickedPoints$click_coords[2, 2]
  
  # Calculate slope and intercept
  slope <- (y2 - y1) / (x2 - x1)
  intercept <- y1 - slope * x1
  
  # Return the parameters as a list
  return(list(slope = slope, intercept = intercept))
}