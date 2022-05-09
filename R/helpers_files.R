#' @importFrom rlang abort warn

# Helper functions to manage file paths and output folders
make_paths <- function(uid, name, output = "data") {
  paths <- list()

  # Output folder for clean data
  paths$clean_output <- here::here(
    output,
    "data-raw",
    glue("{name}-{uid}")
  )

  # Clean files
  paths$clean_files <- here::here(
    paths$clean_output,
    files_clean$filepaths[files_clean$pipeline_id %in% uid]
  )

  # Output folder for raw data
  paths$raw_output <- here::here(
    paths$clean_output,
    "raw"
  )

  # Raw files
  paths$raw_files <- here::here(
    paths$raw_output,
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

  # Check if raw files exist
  exist <- list()
  exist$raw <- lapply(paths$raw_files, file.exists) |>
               unlist() |>
               all()

  # Check if cleaned files exist
  exist$clean <- lapply(paths$clean_files, file.exists) |>
                 unlist() |>
                 any()

  # Messages
  if (ondisk & !exist$raw) msgOndisk(paths) # If data is needed locally, stop process
  if (!ondisk & exist$raw) msgNoload() # If data is downloaded, warning
  if (exist$clean) msgNoclean() # If data is downloaded, warning

  invisible(exist)
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
make_output <- function(uid, name, output = "data") {
  paths <- make_paths(uid, name, output)
  fold <- check_folders(uid, name, output)

  # Create output folders
  type <- pipeline$pipeline_type[pipeline$pipeline_id == uid]
  if (!fold$raw & type == "data") {
    dir.create(paths$raw_output, recursive = TRUE)
  }
  if (!fold$integrated & type == "integrated") {
    dir.create(paths$integrated_output, recursive = TRUE)
  }

  # Create path to raw or integrated data
  if (type == "data") {
    path <- paths$clean_output
  }

  if (type == "integrated") {
    path <- paths$integrated_output
  }

  invisible(path)
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

msgNoclean <- function() {
  rlang::warn(c(
    "The cleaned data is already available on disk, data formatting was thus skipped.",
    "i" = "Delete or move files from disk for data to be downloaded again."
  ))
}

# If data is needed locally, stop process
msgOndisk <- function(paths) {
  rlang::abort(c(
    "This data is unavailable remotely and needs to be on disk for the pipeline to work.",
    "i" = "The raw data should be available at the following paths:",
    "",
    paths$raw_files
  ))
}
