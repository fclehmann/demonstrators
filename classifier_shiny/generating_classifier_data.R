library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)


# Define the API endpoint
# starting the server:
# cd ~/Dokumente/Python_projects/understand_classification/;uvicorn animal_classifier:app --reload

url <- "http://127.0.0.1:8000/predict/"

# List of images to predict
images <- list.files('~/Dokumente/R_projects/demonstrators/classifier_shiny/www/images/fisch/', full.names = T, recursive = T)

#### request image classification ####

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
  animal_classifier_df <- do.call(rbind, content)
  
  colnames(animal_classifier_df) <- c("image_input", "predicted_class_idx", "predicted_class_label", 
                                "output_1", "output_2", "output_3")
  
  # Convert to data frame (ensure it's treated as a data frame)
  animal_classifier_df <- as.data.frame(animal_classifier_df)
} else {
  print(paste("Request failed with status code:", status_code(response)))
}

animal_classifier_df %<>% mutate_all(unlist) %>% as.data.frame()
animal_classifier_df %<>% mutate(true_class = basename(dirname(image_input)))
animal_classifier_df %<>% mutate(prediction_correct = (predicted_class_label == true_class))
save(animal_classifier_df, file = 'data/animal_classifier-last_layer_data.Rdata')


