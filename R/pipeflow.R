#' Execute a complete data workflow
#'
#' This function is used execute an entire data workflow that will download, format, integrate, and incorporate all spatial data into a regular grid to generate a ready to work with integrated dataset. It builds on the functions `pipedat`, `pipeconnect` and `pipegrid` and used a data workflow yaml configuration file to execute the complete the workflow.
#'
#' @param config path to a yaml data workflow configuration file prepared by the user. Use `pipenew()` to generate a new configuration file template.
#'
#' @return This function returns the queried raw data, formatted data, metadata and bibtex associated with the raw data, and all integrated data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pipeflow(config = "./data/config/data_workflow.yaml")
#' }
pipeflow <- function(config) {
  # Load yaml configuration file
  dat <- yaml::read_yaml(config)

  # Parameters
  crs <- dat$data_workflow$parameters$crs
  bbox_pipeline <- unlist(dat$data_workflow$parameters$bbox)
  timespan <- dat$data_workflow$parameters$timespan
  res <- dat$data_workflow$parameters$grid$resolution
  bbox_grid <- unlist(dat$data_workflow$parameters$grid$bbox)

  # Data pipelines
  lapply(
    dat$data_workflow$data_pipeline,
    pipedat,
    crs = crs,
    bbox = bbox_pipeline,
    timespan = timespan
  )

  # # Make grid
  # pipegrid(
  #   bbox_grid,
  #   res
  # )
  #
  # # Data connect
  # lapply(
  #   dat$data_workflow$data_connect,
  #   pipeconnect,
  # )
}
