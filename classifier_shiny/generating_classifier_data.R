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
images <- list.files('~/Dokumente/R_projects/demonstrators/classifier_shiny/www/images/', full.names = T, recursive = T)

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

animal_classifier_df %<>% mutate_all(unlist)# %>% as.data.frame()
animal_classifier_df %<>% mutate(true_class = basename(dirname(image_input)))
animal_classifier_df %<>% mutate(prediction_correct = (predicted_class_label == true_class))
save(animal_classifier_df, file = 'data/animal_classifier-last_layer_data.Rdata')
save(animal_classifier_df, file = 'data/test1.Rdata')

##### creating thumbnails of images ####
# Load libraries
library(imager)
library(purrr)
library(grid)

process_images_sequential <- function(image_paths, target_dir, width = 50, height = 50) {
  # Create a subdirectory named with the resolution (e.g., '50x50') inside the target directory
  target_subdir <- file.path(target_dir, paste0(width, "x", height))
  
  # Create the subdirectory if it doesn't exist
  if (!dir.exists(target_subdir)) {
    dir.create(target_subdir, recursive = TRUE)
  }
  
  # Apply the resize_image function to each image
  resized_image_paths <- purrr::map(image_paths, ~ resize_image(.x, width, height, target_subdir))
  
  # Filter out NULL values (in case some files were skipped)
  resized_image_paths <- Filter(Negate(is.null), resized_image_paths)
  
  return(resized_image_paths)
}

# Function to resize an image (modified for the new subdirectory)
resize_image <- function(image_path, width = 50, height = 50, target_dir) {
  if (file.exists(image_path)) {  # Only process if the file exists
    cat("Processing:", image_path, "\n")  # Print the current image being processed
    
    img <- load.image(image_path)  # Load image using imager package
    img_resized <- resize(img, size_x = width, size_y = height)
    file_name <- basename(image_path)
    target_path <- file.path(target_dir, paste0(file_name))
    save.image(img_resized, target_path)
    
    return(target_path)
  }
  return(NULL)  # If the file doesn't exist, return NULL
}

# Define the target directory for resized images
target_dir <- getwd()  # Target directory for resized images

# Define the desired resolution
width <- 30  # Desired width of the resized images
height <- 30  # Desired height of the resized images

# Call the function to process the images sequentially
resized_image_paths <- process_images_sequential(image_paths, target_dir, width, height)



