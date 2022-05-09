#' @importFrom rlang abort warn

# Helper functions to manage file paths and output folders
make_paths <- function(uid, name, output = "data") {
  paths <- list()
  
  # Output folder for raw data
  paths$raw_output <- here::here(
    output, 
    "data-raw", 
    glue("{name}-{uid}")
  )
  
  # Raw files
  paths$raw_files <- here::here(
    paths$raw_output,
    "raw",
    files_raw$filepaths[files_raw$pipeline_id %in% uid]
  )

  # Output folder for integrated data
  paths$integrated_output <- here::here(
    output, 
    "data-integrated", 
    glue("{name}-{uid}")
  )
  
  invisible(paths)
}


# Check if data is already present and send warning if data is already present and was thus not downloaded, or stop process if data needs to be available locally
check_files <- function(uid, name, output = "data", ondisk = FALSE) {
  # Create paths
  paths <- make_paths(uid, name, output)
  
  # Check if files exist
  cond <- lapply(paths$raw_files, file.exists) |>
          unlist() |>
          all()
          
  # Messages
  if (ondisk & !cond) msgOndisk() # If data is needed locally, stop process
  if (!ondisk & cond) msgNoload() # If data is downloaded, warning 
  
  invisible(cond)
}

# Check if folders exist
check_folders <- function(uid, name, output = "data") {
  paths <- make_paths(uid, name, output)
  out <- list()
  out$raw <- file.exists(paths$raw_output)
  out$integrated <- file.exists(paths$integrated_output)
  invisible(out)
}

# Create output folders for data pipelines
make_output <- function(uid, name, output = "data", ondisk = FALSE) {
  paths <- make_paths(uid, name, output)
  check_files(uid, name, output, ondisk)
  fold <- check_folders(uid, name, output)
  
  # Create output folders
  type <- pipeline$pipeline_type[pipeline$pipeline_id == uid]
  if (!fold$raw & type == "data") {
    dir.create(here::here(paths$raw_output, "raw"), recursive = TRUE)
  } 
  if (!fold$integrated & type == "integrated") {
    dir.create(paths$integrated_output, recursive = TRUE)
  }
  
  invisible(paths)
}

# --------------------------------------------------------------------------------
# Helper messages
# Skip download 
msgNoload <- function() { 
  rlang::warn(c(
    "The data is already available on disk, download was thus skipped.", 
    "i" = "Delete or move files from disk for data to be downloaded again."  
  ))
}


# If data is needed locally, stop process
msgOndisk <- function() { 
  rlang::abort(c(
    "This data is unavailable remotely and needs to be on disk for the pipeline to work.",
    "i" = "The raw data should be available at the following paths:",
    "",
    paths$raw_files
  ))
}
