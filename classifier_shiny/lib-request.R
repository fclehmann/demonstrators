library(httr)
library(httr)
library(curl)
library(digest)

process_urls <- function(input_text) {
  url_list <- strsplit(input_text, "[;\n]")[[1]]
  url_list <- trimws(url_list)
  url_list <- url_list[url_list != ""]
  return(url_list)
}

check_file_size <- function(url, size_limit = 1024, timeout = 4) {
  result <- list()  # Initialize the result list
  
  tryCatch({
    # Send a HEAD request with timeout
    response <- HEAD(url, timeout(timeout))
    
    # Check for successful response
    if (status_code(response) == 200) {
      # Extract content-length header
      file_size <- headers(response)[["content-length"]]
      if (!is.null(file_size)) {
        file_size <- round(as.numeric(file_size)/1024, digits = 0)  # Convert to KB
        result$status <- "Success"
        result$file_size_kb <- file_size
        result$download <- file_size <= size_limit
      } else {
        result$status <- "Size not available"
        result$file_size_kb <- NA
        result$download <- FALSE
      }
    } else {
      result$status <- paste("HTTP status", status_code(response))
      result$file_size_kb <- NA
      result$download <- FALSE
    }
  }, error = function(e) {
    result$status <- paste("Failed to check size:", e$message)
    result$file_size_kb <- NA
    result$download <- FALSE
  })
  
  return(result)
}

download_files <- function(url, target_dir) {
  tryCatch({
    # Generate the file name based on the CRC32 hash of the URL
    file_name <- file.path(target_dir, paste0(digest(url, algo = 'crc32')))
    
    # Download the file
    curl_download(url, file_name)
    cat('Downloaded file: ', file_name, '\n')
    
    # Determine the MIME type of the file
    mime_type <- system(paste0('file --mime-type -b ', file_name), intern = TRUE)
    
    # Define valid MIME types for images
    valid_image_mimes <- c("image/jpeg", "image/png", "image/bmp", "image/tiff")
    
    # Keep the file only if it's an image
    if (mime_type %in% valid_image_mimes) {
      return(file_name)  # Return the full path of the image
    } else {
      # Remove the file if it's not an image
      file.remove(file_name)
      paste("Removed non-image file:", url)
      return(NULL)  # Return NULL for non-image files
    }
    
  }, error = function(e) {
    paste("Failed to download:", url, "-", e$message)
    return(NULL)  # Return NULL in case of an error
  })
}

request_classifier <- function(image_list, API_endpoint = "http://127.0.0.1:8000/predict/") {
  
  # Check if image_list is provided and not empty
  if (length(image_list) == 0) {
    stop("The image_list is empty. Please provide a list of images.")
  }
  
  #### Request image classification ####
  
  # Send the POST request with the list of image inputs
  response <- tryCatch({
    POST(API_endpoint,
         body = list(image_inputs = image_list), 
         encode = "json")
  }, error = function(e) {
    stop("Failed to send request: ", e$message)
  })
  
  # Check the response status and parse the content if successful
  if (status_code(response) == 200) {
    
    # Parse the JSON content
    content_parsed <- content(response, "parsed")
    
    # Extract the first item name dynamically
    item_name <- names(content_parsed)[1]
    content_data <- content_parsed[[item_name]]
    
    # Convert the list of data to a data frame
    animal_classifier_df <- do.call(rbind, content_data)
    colnames(animal_classifier_df) <- c("image_input", "predicted_class_idx", "predicted_class_label", 
                                        "output_1", "output_2", "output_3")
    
    # Convert to a data frame and unlist all columns
    animal_classifier_df <- as.data.frame(animal_classifier_df) %>% select(-predicted_class_idx)
    animal_classifier_df <- animal_classifier_df %>% mutate_all(unlist)
    
  } else {
    stop(paste("Request failed with status code:", status_code(response)))
  }
  
  return(animal_classifier_df)
}








