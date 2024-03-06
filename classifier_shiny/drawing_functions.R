# drawing functions

# Function to plot the scatterplot and dashed line
plotScatter_rbase <- function(inputdata, xlab, ylab) {
  plot(inputdata$Variable1, inputdata$Variable2, col = ifelse(inputdata$Group == "Group1", "blue", "orange"), pch = 19, xlab = xlab, ylab = ylab)
  legend("bottom", legend = c("Group 1", "Group 2"), col = c("blue", "orange"), pch = 19, title = "", bg = "white", inset = 0.02)
}

# ggplot Version of function to plot the scatterplot and dashed line
plotScatter_ggplot <- function(inputdata, xlab, ylab) {
  base_plot <- ggplot() +
    geom_point(data = inputdata, aes(x = Variable1, y = Variable2, color = Group), size=3) +
    labs(x = xlab, y = ylab) +
    theme_minimal() +
    theme(
      text = element_text(size = 20),  # Adjust the size as needed
      legend.title = element_blank(),  # Remove legend title
      legend.position = "bottom",      # Move legend to bottom
      legend.direction = "horizontal"
    ) +
    scale_color_manual(values = c("blue", "orange"))
  return(base_plot)
}