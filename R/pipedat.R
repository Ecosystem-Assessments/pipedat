#' Execute data pipelines
#'
#' This function is used to access, load and format a wide variety
#' of data; this function calls on a series of individual scripts
#' built to access data programmatically and reproducibly, which
#' we refer to as *data pipelines*.
#'
#' @param uid unique identifier of queried data. The full list of available data pipelines can be consulted using `pipelines()`
#' @eval dp_params()
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
pipedat <- function(uid, bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Execute data pipelines
  lapply(
    uid,
    function(x) {
      do.call(
        glue("dp_{x}"),
        list(
          uid = uid,
          bbox = bbox,
          bbox_crs = bbox_crs,
          timespan = timespan
        )
      )
    }
  )
}
