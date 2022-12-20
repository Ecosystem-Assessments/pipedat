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
#' pipeflow(config = "./data/config/pipedat.yml")
#' }
pipeflow <- function(config) {
  # Load yaml configuration file
  dat <- yaml::read_yaml(config)

  # Parameters
  crs <- dat$pipedat$parameters$crs
  bbox <- unlist(dat$pipedat$parameters$bbox)
  cellsize <- dat$pipedat$parameters$cellsize
  aoi <- dat$pipedat$parameters$aoi
  timespan <- dat$pipedat$parameters$timespan
  params <- dat$pipedat$params

  # Grid
  if (
    dat$pipedat$parameters$make_grid &
    !file.exists(here::here("data","grid","grid.tif"))
  ) {
    if (is.na(aoi) | is.null(aoi)) {
      aoi <- bbox
    } else {  
      aoi <- sf::st_read(aoi)
    }
    pipegrid(aoi, cellsize, crs)    
  }
  # 
  # # Grid figure
  # if (file.exists("data/data-grid/")) {
  #   plotgrid()
  # }

  # Data pipelines
  if (!is.null(dat$pipedat$data_pipeline)) {
    args <- c(
      list(
        uid = dat$pipedat$data_pipeline,
        bbox = bbox,
        bbox_crs = crs,
        timespan = timespan
      ),
      params
    )
    do.call(pipedat, args)
  }

  # Integration pipelines
  if (!is.null(dat$pipedat$data_integration)) {
    args <- c(
      list(
        uid = dat$pipedat$data_integration,
        bbox = bbox,
        bbox_crs = crs,
        timespan = timespan
      ),
      params
    )
    do.call(pipedat, args)
  }


  # # Integrated data figures
  # if (!is.null(dat$pipedat$data_integration)) {
  #   plotdat(dat$pipedat$data_integration)
  # }
}
