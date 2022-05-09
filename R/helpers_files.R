# Helper functions to manage file paths and output folders
# Create output folders for data pipelines
make_output <- function(uid, name, output = "data", type) {
  if (type == "data") {
    path <- here::here(output, "data-raw", glue("{name}-{uid}"))
    dir.create(here::here(path, "raw"), recursive = TRUE)
  }

  if (type == "integrated") {
    path <- here::here(output, "data-integrated", glue("{name}-{uid}"))
    dir.create(path, recursive = TRUE)
  }

  # Return output path
  invisible(path)
}

# ------------------------------------------------------------------------------
# Helper messages / check functions
# Data needed locally
check_data <- function(filepath, path) {
  if (!file.exists(filepath)) {
    stop(glue("This data is unavailable remotely. The raw data needs to be manually inserted in the folder `{path}/raw/` for the pipeline to work."))
  }
}

