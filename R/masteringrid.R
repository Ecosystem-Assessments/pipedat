#' Generic function incorporate data in study grid
#'
#' This function is used to incorporate data in a study grid in a generic way, *e.g.* by
#' warping raster data to a new raster object with a different resolution.
#' user-specified study grid, if applicable, which we refer to as *data integration pipelines*.
#'
#' @param dat object with spatial data to import in study grid, either `sf` or `stars` object
#' @param grd spatial grid used for data integration. Can be a `sf` object containing polygons or a `stars` rasters that will be used as a template. Individual cells must be identified by a unique identifier called `uid`. If NULL, the function attempts to load a grids `./project-data/data-grid/grid_raster.tif` or `./project-data/data-grid/grid.poly.geojson`.
#'
#' @return A data.frame with columns `uid` related to study grid unique identifier and a column with named `name` with the values imported
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pipedat("0001")
#' }
#' @export
masteringrid <- function(dat, grd = here::here("project-data", "grid", "grid.tif")) {
  # Get grid
  if (inherits(grd, "character")) grd <- stars::read_stars(grd)
  if (!inherits(grd, "stars")) grd <- stars::st_as_stars(grd)

  # stars objects
  if (inherits(dat, "stars")) {
    stars::st_warp(dat, grd)
  }

  # # sf objects
  # if ("sf" %in% class(dat)) {
  #   exactextract::exact_extract() <- works with raster package
  #   fasterize::fasterize() <- works with raster package
  #   stars::st_rasterize() <- works directly with stars (obviously)
  # }
}

#' @describeIn masteringrid Function to gather all tif files generated in a pipedat projects
#' @export
gather_ingrid <- function() {
  # Locate files
  files <- dir(here::here("project-data", "pipedat"), recursive = TRUE, full.names = TRUE)
  iid <- stringr::str_detect(files, "ingrid")
  files <- files[iid]

  # Copy files in same location
  out <- here::here("project-data", "pipegrid")
  chk_create(out)
  file.copy(
    from = files,
    to = here::here(out, basename(files)) # ,
    # recursive = TRUE
  )
}
