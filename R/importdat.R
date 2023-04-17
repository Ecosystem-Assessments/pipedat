#' Imports formatted and integrated data in global R environment
#'
#' This function is used to import formatted and integrated data
#' in the global R environment for use in other pipelines or
#' user processes.
#'
#' @param uid unique identifier of queried data. The full list of available data pipelines can be consulted using `pipelines()`
#' @param type type of data to import, one of "format" or "integrated"
#'
#' @return This function returns objects in the global R environment
#'
#' @export
#'
#' @examples
#' \dontrun{
#' importdat("f635934a")
#' }
#' @export
importdat <- function(uid, type) {
  paths <- get_filepaths(uid)[type][[1]]
  dat <- lapply(paths, masterload)
  names(dat) <- basename(paths)
  invisible(dat)
}
