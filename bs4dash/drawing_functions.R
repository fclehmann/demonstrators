# drawing functions

# Function to plot the scatterplot and dashed line
plotScatterAndLine <- function(inputdata, group, clickedPoints, mouse_position, line_color, line_width) {
  plot(inputdata$Variable1, inputdata$Variable2, col = ifelse(group == "Group 1", "blue", "orange"), pch = 19, xlab = "", ylab = "")
  
  legend("bottom", legend = c("Group 1", "Group 2"), col = c("blue", "orange"), pch = 19, title = "", bg = "white", inset = 0.02)
  
  if (!clickedPoints$line_exists && nrow(clickedPoints$click_coords) < 2 && nrow(clickedPoints$click_coords) > 0 && !is.null(mouse_position())) {
    points(clickedPoints$click_coords[1, 1], clickedPoints$click_coords[1, 2], col = line_color, pch = 4, cex = 2)
    lines(c(clickedPoints$click_coords[1, 1], mouse_position()$x), c(clickedPoints$click_coords[1, 2], mouse_position()$y), col = line_color, lty = 2, lwd = line_width)
  }
  
  if (nrow(clickedPoints$click_coords) >= 2) {
    points(clickedPoints$click_coords[1, 1], clickedPoints$click_coords[1, 2], col = line_color, pch = 4, cex = 2)
    points(clickedPoints$click_coords[2, 1], clickedPoints$click_coords[2, 2], col = line_color, pch = 4, cex = 2)
    lines(c(clickedPoints$click_coords[1, 1], clickedPoints$click_coords[2, 1]), c(clickedPoints$click_coords[1, 2], clickedPoints$click_coords[2, 2]), col = "red", lty = 2, lwd = line_width)
    clickedPoints$line_exists <- TRUE
  }
}

# ggplot Version of function to plot the scatterplot and dashed line
plotScatterAndLine_ggplot <- function(inputdata, clickedPoints, mouse_position, line_color = "red", line_width = 2) {
  base_plot <- ggplot() +
    geom_point(data = inputdata, aes(x = Variable1, y = Variable2, color = Group), size=3) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(
      text = element_text(size = 20),  # Adjust the size as needed
      legend.title = element_blank(),  # Remove legend title
      legend.position = "bottom",      # Move legend to bottom
      legend.direction = "horizontal"
    ) +
    scale_color_manual(values = c("blue", "orange"))
  
  # Connect the last clicked point to the mouse pointer position with a dashed line
  if (!clickedPoints$line_exists && nrow(clickedPoints$click_coords) < 2 && nrow(clickedPoints$click_coords) > 0 && !is.null(mouse_position())) {
    base_plot <- base_plot +
      geom_point(aes(
        x = clickedPoints$click_coords[1, 1],
        y = clickedPoints$click_coords[1, 2]
      ),
      color = line_color,
      size = 5,
      shape = 4
      )  +
      geom_line(aes(
        x = c(clickedPoints$click_coords[1, 1], mouse_position()$x),
        y = c(clickedPoints$click_coords[1, 2], mouse_position()$y)
      ),
      color = line_color,
      linetype = "dashed",
      linewidth = line_width
      )
  }
  
  if (nrow(clickedPoints$click_coords) >= 2) {
    base_plot <- base_plot +
      geom_point(aes(
        x = clickedPoints$click_coords[1, 1],
        y = clickedPoints$click_coords[1, 2]
      ),
      color = line_color,
      size = 5,
      shape = 4
      )  +
      geom_point(aes(
        x = clickedPoints$click_coords[2, 1],
        y = clickedPoints$click_coords[2, 2]
      ),
      color = line_color,
      size = 5,
      shape = 4
      ) +
      geom_line(aes(
        x = c(clickedPoints$click_coords[1, 1], clickedPoints$click_coords[2, 1]),
        y = c(clickedPoints$click_coords[1, 2], clickedPoints$click_coords[2, 2])
      ),
      color = line_color,
      linetype = "dashed",
      linewidth = line_width
      )
    clickedPoints$line_exists <- TRUE
  }
  
  return(base_plot)
}