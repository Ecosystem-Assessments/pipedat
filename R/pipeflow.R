#' Execute a complete data workflow
#'
#' This function is used execute an entire data workflow that will download, format, integrate, and incorporate all spatial data into a regular grid to generate a ready to work with integrated dataset. It uses a data workflow yaml configuration file to execute the complete the workflow.
#'
#' @param config path to a yaml data workflow configuration file prepared by the user. Use `pipenew()` to generate a new configuration file template. Alternatively, this can be a list organized as a yaml document.
#'
#' @return This function returns the queried raw data, formatted data, metadata and bibtex associated with the raw data, and all integrated data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pipeflow(config = "./workflow.yaml")
#' }
pipeflow <- function(config) {
  # Load yaml configuration file
  dat <- yaml::read_yaml(config)

  # Parameters
  crs <- dat$data_workflow$parameters$crs
  bbox <- unlist(dat$data_workflow$parameters$bbox)
  timespan <- dat$data_workflow$parameters$timespan
  params <- dat$data_workflow$params

  # Data pipelines
  if (!is.null(dat$data_workflow$data_pipeline)) {
    args <- c(
      list(
        uid = dat$data_workflow$data_pipeline,
        bbox = bbox,
        bbox_crs = crs,
        timespan = timespan
      ),
      params
    )
    do.call(pipedat, args)
  }

  # Integration pipelines
  if (!is.null(dat$data_workflow$data_integration)) {
    args <- c(
      list(
        uid = dat$data_workflow$data_integration,
        bbox = bbox,
        bbox_crs = crs,
        timespan = timespan
      ),
      params
    )
    do.call(pipedat, args)
  }

  # # Grid
  # pipegrid(
  #
  # )

  # Grid figure
  if (file.exists("data/data-grid/")) {
    plotgrid()    
  }

  # Integrated data figures
  if (!is.null(dat$data_workflow$data_integration)) {
    plotdat(dat$data_workflow$data_integration)
  }
}
