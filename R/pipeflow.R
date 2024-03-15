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
#' pipeflow(config = "./project-data/config/pipedat.yml")
#' }
pipeflow <- function(config = "./project-data/pipedat/pipeflow.yml") {
  # Load yaml configuration file
  dat <- yaml::read_yaml(config)

  # Parameters
  crs <- dat$pipedat$parameters$crs
  bbox <- unlist(dat$pipedat$parameters$bbox)
  timespan <- dat$pipedat$parameters$timespan
  cellsize <- dat$pipedat$parameters$cellsize
  aoi <- dat$pipedat$parameters$aoi
  grd <- dat$pipedat$params$grid
  integrate <- dat$pipedat$parameters$integrate
  ingrid <- dat$pipedat$parameters$ingrid
  params <- dat$pipedat$params

  # Grid
  if (
    dat$pipedat$parameters$make_grid &
      !file.exists(here::here("project-data", "grid", "grid.tif"))
  ) {
    if (is.na(aoi) | is.null(aoi)) {
      aoi <- bbox
    } else {
      aoi <- sf::st_read(aoi)
    }
    pipegrid(aoi, cellsize, crs)
  }

  # Data pipelines
  if (!is.null(dat$pipedat$pipelines)) {
    args <- c(
      list(
        uid = dat$pipedat$pipelines,
        bbox = bbox,
        timespan = timespan,
        integrate = integrate,
        grd = grd,
        ingrid = ingrid
      ),
      params
    )
    do.call(pipedat, args)
  }

  # # Gather all data in grid from project
  # gather_ingrid()

  # Metadata
  gather_bib()
  # gather_meta()

  # # Figures
  # pipeplot()

  # # Render report
  # pipereport()
}
