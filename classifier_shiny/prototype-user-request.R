library(shiny)
library(httr)
library(curl)
library(DT)

source('lib-request.R')

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
      'https://cdn.pixabay.com/photo/2024/05/28/08/06/sheep-8793142_1280.jpg', 
      'https://cdn.pixabay.com/photo/2017/02/09/18/40/seal-2053165_1280.jpg',
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
  tableOutput("status_output"), 
  DTOutput("data_table")
)

# --- SHINY SERVER ---
server <- function(input, output, session) {
  
  # Define the parent directory for custom temporary files
  tmp_parent <- "www/tmp-session/"  # You can change this to your desired location
  
  # Create a unique directory for each session using session token (or ID)
  user_dir <- file.path(tmp_parent, session$token)
  dir.create(path = user_dir)
  cat('tmp-dir: ', user_dir, '\n')
  
  # Observe download button click
  observeEvent(input$download_btn, {
    # Process input URLs
    url_list <- process_urls(input$url_input)
    
    # Check url file sizes
    user_request <- lapply(X = url_list, FUN = check_file_size)
    names(user_request) <- url_list
    user_request <- do.call(rbind, lapply(user_request, as.data.frame))
    user_request$url <- rownames(user_request)
    rownames(user_request) <- NULL
    download_list <- user_request %>% filter(download == TRUE) %>% pull(url) %>% as.list()
    
    lapply(X = download_list, FUN = download_files, target_dir = user_dir)  
    
    erg <- request_classifier(image_list = normalizePath(list.files(user_dir, full.names = T, recursive = T)))
    output$data_table <- renderDT({
      datatable(
        erg, # The reactive data frame
        options = list(
          pageLength = 5,  # Show 5 rows per page
          autoWidth = TRUE # Adjust column widths automatically
        ),
        rownames = FALSE # Hide row names
      )
    })
    output$status_output <- renderTable({
      user_request
    })
    
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
