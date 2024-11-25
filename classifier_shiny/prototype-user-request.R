library(shiny)
library(httr)
library(curl)
library(mime)

# --- FUNCTION: Process Input URLs ---
process_urls <- function(input_text) {
  url_list <- strsplit(input_text, "[;\n]")[[1]]
  url_list <- trimws(url_list)
  url_list <- url_list[url_list != ""]
  return(url_list)
}

# --- FUNCTION: Create Directory ---
create_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# --- FUNCTION: Check File Size ---
check_file_size <- function(url, size_limit = 500, timeout = 4) {
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

download_files <- function(url_list, target_dir) {
  tryCatch({
    # Download the file
    file_name <- file.path(target_dir, url)
    curl_download(url, file_name)
    
    # Get MIME type using the mime package
    mime_type <- mime::mime(file_name)
    
    # Define valid MIME types for images
    valid_image_mimes <- c("image/jpeg", "image/png", "image/bmp", "image/tiff")
    
    # Keep the file only if it's an image
    if (mime_type %in% valid_image_mimes) {
      paste("Downloaded and kept:", url)
    } else {
      # Remove the file if it's not an image
      file.remove(file_name)
      paste("Removed non-image file:", url)
    }
    
  }, error = function(e) {
    paste("Failed to download:", url, "-", e$message)
  })
}


# --- FUNCTION: Delete All Files ---
delete_files <- function(target_dir) {
  if (dir.exists(target_dir)) {
    files <- list.files(target_dir, full.names = TRUE)
    file.remove(files)
    return(paste(length(files), "files deleted from", target_dir))
  } else {
    return("Directory does not exist.")
  }
}

# --- SHINY UI ---
ui <- fluidPage(
  textAreaInput(
    inputId = "url_input",
    label = "Enter Image URLs (separated by ; or newline):",
    value = paste(
      "https://media.istockphoto.com/id/1652227141/de/foto/laufender-flauschiger-pembroke-welsh-corgi-welpe.jpg?s=612x612&w=0&k=20&c=EpSVPET6e-VuKjcrMVfANWKnLsft6QCTDvZFjioDg4k=",
      "https://www.istockphoto.com/de/foto/laufender-flauschiger-pembroke-welsh-corgi-welpe-gm1652227141-534204006?searchscope=image%2Cfilm",
      "https://pixabay.com/de/photos/image-9215914/",
      "https://cdn.pixabay.com/photo/2024/11/22/10/03/canyon-9215914_1280.jpg",
      sep = "\n"
    ),
    rows = 5
  ),
  actionButton(
    inputId = "download_btn",
    label = "Download Images"
  ),
  actionButton(
    inputId = "delete_btn",
    label = "Delete All Files"
  ),
  verbatimTextOutput("status_output")
)

# --- SHINY SERVER ---
server <- function(input, output, session) {
  
  # Define the parent directory for custom temporary files
  tmp_parent <- "www/tmp-session/"  # You can change this to your desired location
  
  # Create a unique directory for each session using session token (or ID)
  user_dir <- file.path(tmp_parent, session$token)
  create_dir(dir_path = user_dir)
  cat('tmp-dir: ', user_dir, '\n')
  
  # Observe download button click
  observeEvent(input$download_btn, {
    # Process input URLs
    url_list <- process_urls(input$url_input)
    
    # Check url file sizes
    user_request <- lapply(X = url_list, FUN = check_file_size, size_limit = 300)
    names(user_request) <- url_list
    user_request <- do.call(rbind, lapply(user_request, as.data.frame))
    user_request$url <- rownames(user_request)
    rownames(user_request) <- NULL
    
    
    output$status_output <- renderPrint({ user_request })
    
    # Download files
    #status <- download_files(url_list, target_dir = user_dir)
    # output$status_output <- renderPrint({
    #   c(unlist(sizes), unlist(status))
    # })
  })
  
  # Observe delete button click
  observeEvent(input$delete_btn, {
    status <- delete_files(target_dir = user_dir)
    output$status_output <- renderPrint({ status })
  })
  
  # Cleanup: delete temporary files when the session ends
  session$onSessionEnded(function() {
    if (dir.exists(paths = user_dir)) {
      unlink(user_dir, recursive = TRUE)
      message("Temporary files deleted for session:", session$token)
    }
  })
}

shinyApp(ui, server)
