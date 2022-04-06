#' Shortname of dataset to be queried (#{{ out$uid }})
#'
#' Short description of the dataset to be queried through this data pipeline
#'
#' @param output output folder for queried data. That folder will be organized into the different files that are loaded, i.e. raw data, formatted data, metadata and bibtex files.
#' @param bbox bounding box to spatially subset the queried data, if applicable. The bounding box should be of the form `c(xmin, ymin, xmax, ymax)`
#' @param timespan time span to spatially subset the queried data, if applicable. The time span should be of the form `c(time_from, time_to)`
#'
#' @rdname pipelines
#'
#' @examples
#' \dontrun{
#' pd_{{ out$uid }}()
#' }

pd_{{ out$uid }} <- function(output = NULL, bbox = NULL, timespan = NULL, ...) {
  # Load the data 
  
  # Format the data 
  
  # Create metadata 
  
  # Create bibtex
  
  # --------------------------------
  # Export 
  otp <- glue("data/data-format/{ {{ out$uid }} }.R")
  
}
