#' Execute data pipelines
#'
#' This function is used to access, load and format a wide variety 
#' of data; this function calls on a series of individual scripts 
#' built to access data programmatically and reproducibly, which 
#' we refer to as *data pipelines*.
#'
#' @param uid unique identifier of queried data. The full list of available data pipelines can be consulted using `pipelines()`
#' @param output output folder for queried data. That folder will be organized into the different files that are loaded, i.e. raw data, formatted data, metadata and bibtex files.
#' @param bbox bounding box to spatially subset the queried data, if applicable. The bounding box should be of the form `c(xmin, ymin, xmax, ymax)`
#' @param timespan time span to spatially subset the queried data, if applicable. The time span should be of the form `c(time_from, time_to)`
#'
#' @return This function returns the queried raw data, formatted data, metadata and bibtex associated with the raw data. 
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pipedat("0001")
#' }
pipedat <- function(uid, output = NULL, bbox = NULL, timespan = NULL) {  
  # Output folders 
  makeOutput(uid, output)
    
  # Execute data pipelines
  do.call(glue("dp_{uid}"), list(uid = uid, bbox = bbox, timespan = timespan))
}


