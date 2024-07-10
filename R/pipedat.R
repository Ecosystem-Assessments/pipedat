#' Execute data pipelines
#'
#' This function is used to access, load and format a wide variety
#' of data; this function calls on a series of individual scripts
#' built to access data programmatically and reproducibly, which
#' we refer to as *data pipelines*.
#'
#' @param uid unique identifier of queried data. The full list of available data pipelines can be consulted using `pipelist()`
#' @eval dp_params()
#' @param ... further arguments used in functions, if applicable.
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
pipedat <- function(uid, bbox = NULL, timespan = NULL, integrate = TRUE, grd = "project-data/grid/grid.tif", ...) {
  # Execute data pipelines
  lapply(
    uid,
    function(x) {
      do.call(
        pipecode[[x]],
        list(
          uid = uid,
          bbox = bbox,
          timespan = timespan,
          integrate = integrate,
          grd = grd,
          ...
        )
      )
    }
  )
}
