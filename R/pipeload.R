#' Execute data pipelines
#'
#' This function is used to access, load and format a wide variety
#' of data; this function calls on a series of individual scripts
#' built to access data programmatically and reproducibly, which
#' we refer to as *data pipelines*.
#'
#' @param urls urls for data download.
#' @param govcan unique identifier of resource on the open government federal data portal to download.
#' @param output output folder for queried data.
#' @param large logical, whether file to download is large and thus R options should be modified for longer timeout options. Default set to `FALSE`.
#'
#' @return This function returns the queried raw data, formatted data, metadata and bibtex associated with the raw data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' govcan <- "23eb8b56-dac8-4efc-be7c-b8fa11ba62e9"
#' output <- "project-data/"
#' if (file.exists(output)) dir.create(output)
#' pipeload(govcan = govcan, output = output)
#' }
#' @export
# Generic function to download data from url
pipeload <- function(urls = NULL, govcan = NULL, output, large = FALSE) {
  # Default R options limit download time to 60 seconds. Modify for larger files
  if (large) {
    old <- getOption("timeout")
    on.exit(options(timeout = old), add = TRUE)
    options(timeout = 30000)
  }

  if (!is.null(urls)) {
    lapply(
      urls,
      function(x) curl::curl_download(x, destfile = here::here(output, basename(x)))
    )
  }

  if (!is.null(govcan)) {
    rgovcan::govcan_setup()
    try(
      rgovcan::govcan_dl_resources(
        resources = govcan,
        path = output
      )
    )
  }

  # Unzip
  zipfiles <- dir(output, pattern = ".zip", full.names = TRUE)
  lapply(
    zipfiles,
    function(x) {
      archive::archive_extract(
        archive = x,
        dir = output
      )
    }
  )
}
