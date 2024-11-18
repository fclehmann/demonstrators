library(httr)
library(jsonlite)

# Define the API endpoint
url <- "http://127.0.0.1:8000/predict/"

# List of images to predict
setwd('~/Dokumente/Python_projects/understand_classification/traindata/vogel/')
images <- list.files('~/Dokumente/Python_projects/understand_classification/traindata//', full.names = T, recursive = T)

# Send the POST request with the list of image inputs
response <- POST(
  url,
  body = list(image_inputs = images),  # Pass the images as JSON body
  encode = "json"                     # Automatically encode the body as JSON
)


# Check the response status and parse the content if successful
if (status_code(response) == 200) {
  # Parse the JSON content
  item_name <- names(content(response, "parsed"))[1]
  content <- content(response, "parsed")[[item_name]]
  predictions_df <- do.call(rbind, content)
  
  colnames(predictions_df) <- c("image_input", "predicted_class_idx", "predicted_class_label", 
                                "output_1", "output_2", "output_3")
  
  # Convert to data frame (ensure it's treated as a data frame)
  predictions_df <- as.data.frame(predictions_df)
} else {
  print(paste("Request failed with status code:", status_code(response)))
}

library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)

predictions_df %<>% mutate_all(unlist)#



predictions_df %>% ggplot() +
  geom_point(aes(x = output_1, y = output_3, colour = predicted_class_label))
