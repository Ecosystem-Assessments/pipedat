#' Execute data pipelines
#'
#' This function is used to access, load and format a wide variety
#' of data; this function calls on a series of individual scripts
#' built to access data programmatically and reproducibly, which
#' we refer to as *data pipelines*.
#'
#' @param uid unique identifier of queried data. The full list of available data pipelines can be consulted using `pipelines()`
#' @eval dp_params()
#' @eval di_params()
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
pipedat <- function(uid, bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, ...) {
  # Pipeline type
  type <- get_pipeline_type(uid)
  data_pipeline <- type == "data"
  integration_pipeline <- type == "integration"

  # Execute data pipelines
  lapply(
    uid[data_pipeline],
    function(x) {
      do.call(
        pipecode[[x]],
        list(
          uid = uid[data_pipeline],
          bbox = bbox,
          bbox_crs = bbox_crs,
          timespan = timespan,
          ...
        )
      )
    }
  )

  # Execute data integration pipelines
  lapply(
    uid[integration_pipeline],
    function(x) {
      do.call(
        glue("di_{x}"),
        list(
          uid = uid[integration_pipeline],
          bbox = bbox,
          bbox_crs = bbox_crs,
          timespan = timespan,
          grid = grid,
          ...
        )
      )
    }
  )
}
