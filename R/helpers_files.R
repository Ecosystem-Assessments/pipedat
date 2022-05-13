#' @importFrom rlang abort warn
#' @importFrom cli symbol
#' @importFrom crayon blue

# Helper functions to manage file paths and output folders
make_paths <- function(uid, name) {
  paths <- list()

  # Output folder for clean data
  paths$clean_output <- here::here(
    "data",
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
    "data",
    "data-integrated",
    glue("{name}-{uid}")
  )

  # Integrated files
  paths$integrated_files <- here::here(
    paths$integrated_output,
    files_integrated$filepaths[files_integrated$pipeline_id %in% uid]
  )

  invisible(paths)
}


# Check if data is already present and send warning if data is already present and was thus not downloaded, or stop process if data needs to be available locally
check_files <- function(uid, name, ondisk = FALSE) {
  # Create paths
  paths <- make_paths(uid, name)

  # Check if raw files exist
  exist <- list()
  if (length(paths$raw_files > 0)) {
    exist$raw <- lapply(paths$raw_files, file.exists) |>
      unlist() |>
      all()
  } else {
    exist$raw <- FALSE
  }

  # Check if cleaned files exist
  if (length(paths$clean_files > 0)) {
    exist$clean <- lapply(paths$clean_files, file.exists) |>
      unlist() |>
      any()
  } else {
    exist$clean <- FALSE
  }

  # Check if integrated files exist
  if (length(paths$integrated_files > 0)) {
    exist$integrated <- lapply(paths$integrated_files, file.exists) |>
      unlist() |>
      all()
  } else {
    exist$integrated <- FALSE
  }

  # Messages
  if (ondisk & !exist$raw) msgOnDisk(uid, name, paths) # If data is needed locally, stop process
  if (!ondisk & exist$raw) msgNoLoad(uid, name) # If data is downloaded, warning
  if (exist$clean) msgNoClean(uid, name) # If data is downloaded, warning
  if (exist$integrated) msgNoIntegration(uid, name) # If data is downloaded, warning

  invisible(exist)
}

# Check if folders exist
check_folders <- function(uid, name) {
  paths <- make_paths(uid, name)
  out <- list()
  out$raw <- file.exists(paths$raw_output)
  out$integrated <- file.exists(paths$integrated_output)
  invisible(out)
}

# Create output folders for data pipelines
make_output <- function(uid, name) {
  paths <- make_paths(uid, name)
  fold <- check_folders(uid, name)

  # Create output folders
  type <- pipeline$pipeline_type[pipeline$pipeline_id == uid]
  if (!fold$raw & type == "data") {
    dir.create(paths$raw_output, recursive = TRUE)
  }
  if (!fold$integrated & type == "integration") {
    dir.create(paths$integrated_output, recursive = TRUE)
  }

  # Create path to raw or integrated data
  if (type == "data") {
    path <- paths$clean_output
  }

  if (type == "integration") {
    path <- paths$integrated_output
  }

  invisible(path)
}

# --------------------------------------------------------------------------------
# Helper messages
# Skip download
msgNoLoad <- function(uid, name) {
  rlang::warn(c(
    glue("The data for the {name} pipeline (id: {uid}) is already available on disk, download was thus skipped."),
    "i" = "Delete or move files from disk for data to be downloaded again."
  ))
}

msgNoClean <- function(uid, name) {
  rlang::warn(c(
    glue("The cleaned data from the {name} pipeline (id: {uid}) is already available on disk, data formatting was thus skipped."),
    "i" = "Delete or move files from disk for data to be downloaded again."
  ))
}

# If data is needed locally, stop process
msgOnDisk <- function(uid, name, paths) {
  rlang::abort(c(
    glue("The data for the {name} pipeline (id: {uid}) is unavailable remotely and needs to be on disk for the pipeline to work."),
    "i" = "The raw data should be available at the following paths:",
    "",
    paths$raw_files
  ))
}

msgNoIntegration <- function(uid, name) {
  rlang::warn(c(
    glue("The integrated data from the {name} pipeline (id: {uid}) is already available on disk, data integration was thus skipped."),
    "i" = "Delete or move files from disk for data to be downloaded again."
  ))
}



msgInfo <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$info, ...)
  message(crayon::blue(txt), appendLF = appendLF)
  invisible(txt)
}
