#' Execute data pipelines
#'
#' This function is used to access, load and format a wide variety
#' of data; this function calls on a series of individual scripts
#' built to access data programmatically and reproducibly, which
#' we refer to as *data pipelines*.
#'
#' @param uid unique identifier of queried data. The full list of available data pipelines can be consulted using `pipelines()`
#' @eval doc_params()
#' @param urls urls for data download
#' @param govcan unique identifier of resource on the open government federal data portal to download
#'
#' @return This function returns the queried raw data, formatted data, metadata and bibtex associated with the raw data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pipedat("0001")
#' }
#' @export
#' @describeIn pipedat execute data pipelines
pipedat <- function(uid, name = NULL, output = NULL, crs = 4326, bbox = NULL, timespan = NULL, ...) {
  # Execute data pipelines
  do.call(
    glue("dp_{uid}"),
    list(
      uid = uid,
      name = name,
      output = output,
      crs = crs,
      bbox = bbox,
      timespan = timespan
    )
  )
}

# ------------------------------------------------------------------------------
#' @describeIn pipedat download data from url or open government federal portal
#' @export
# Generic function to download data from url
pipeload <- function(urls = NULL, govcan = NULL, output) {
  if (!is.null(urls)) {
    lapply(
      urls,
      function(x) curl::curl_download(x, destfile = glue("{output}{basename(x)}"))
    )
  }

  if (!is.null(govcan)) {
    rgovcan::govcan_setup()
    rgovcan::govcan_dl_resources(
      resources = govcan,
      path = output
    )
  }

  # Unzip
  zipfiles <- dir(output, pattern = ".zip", full.names = TRUE)
  lapply(zipfiles, function(x) utils::unzip(x, exdir = output))
}