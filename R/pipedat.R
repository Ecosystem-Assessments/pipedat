#' Execute data pipelines
#'
#' This function is used to access, load and format a wide variety
#' of data; this function calls on a series of individual scripts
#' built to access data programmatically and reproducibly, which
#' we refer to as *data pipelines*.
#'
#' @param uid unique identifier of queried data. The full list of available data pipelines can be consulted using `pipelines()`
#' @eval my_params()
#'
#' @return This function returns the queried raw data, formatted data, metadata and bibtex associated with the raw data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pipedat("0001")
#' }
pipedat <- function(uid, output = NULL, input = NULL, bbox = NULL, timespan = NULL) {
  # Output folders
  output <- checkOutput()
  makeOutput(uid, output)

  # Execute data pipelines
  do.call(glue("dp_{uid}"), 
          list(
            uid = uid, 
            output = output, 
            input = input, 
            bbox = bbox, 
            timespan = timespan
          )
        )
}


# ------------------------------------------------------------------------------
# Check if output ends with a "/" to create proper path
checkOutput <- function(output = NULL) {
  if( !is.null(output)) {
    nc <- nchar(output)
    last_char <- ifelse(substr(output, nc, nc) == "/", TRUE, FALSE)
    ifelse(last_char, output, glue("{output}/"))
  } else {
    "data/"
  }
}


# ------------------------------------------------------------------------------
# Create output folders for data pipelines
makeOutput <- function(uid, output = NULL) {
  # Output
  output <- ifelse(is.null(output), "data/", output)

  # Check if output ends with a "/" to create proper path
  out <- checkOutput(output)

  # Names of output folders
  l <- list(
    glue("{out}data-raw/{uid}/"),
    glue("{out}data-format/{uid}/"),
    glue("{out}data-metadata/"),
    glue("{out}data-bib/")
  )

  # Create folders if they do not exist
  # for(i in 1:length(l)) if (!file.exists(l[[i]])) dir.create(l[[i]], recursive = TRUE)
  invisible(
    lapply(l, function(x) if (!file.exists(x)) dir.create(x, recursive = TRUE))
  )
  
  # TODO: For GitHub, create .gitkeep and modify .gitignore
}

# ------------------------------------------------------------------------------
# Generic function to download data from url
pipeload <- function(urls, output) {
  # Download 
  lapply(urls, function(x) curl::curl_download(x, destfile = glue("{output}{basename(x)}")))
  
  # Unzip 
  zipfiles <- dir(output, pattern = ".zip", full.names = TRUE)
  lapply(zipfiles, function(x) unzip(x, exdir = output))
}

