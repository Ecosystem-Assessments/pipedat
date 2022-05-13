#' Execute data integration pipelines
#'
#' This function is used to format, analyze, integrate and incorporate data into a
#' user-specified study grid, if applicable, which we refer to as *data integration pipelines*.
#'
#' @param uid unique identifier of the data integration pipeline. The full list of available data pipelines can be consulted using `pipelines(type = "integrated")`
#' @eval di_params()
#'
#' @return This function returns the integrated data and associated metadata.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pipeint("0001")
#' }
#' @export
pipein <- function(uid, grid = NULL, ...) {
  # Execute data integration pipelines
  # Execute data pipelines
  lapply(
    uid,
    function(x) {
      do.call(
        glue("di_{x}"),
        list(
          uid = uid,
          grid = grid
        )
      )
    }
  )
}
