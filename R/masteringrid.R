#' Generic function incorporate data in study grid
#'
#' This function is used to incorporate data in a study grid in a generic way, *e.g.* by
#' warping raster data to a new raster object with a different resolution.
#' user-specified study grid, if applicable, which we refer to as *data integration pipelines*.
#'
#' @param dat object with spatial data to import in study grid, either `sf` or `stars` object
#' @param grid spatial grid used for data integration. Can be a `sf` object containing polygons or a `stars` rasters that will be used as a template. Individual cells must be identified by a unique identifier called `uid`. If NULL, the function attempts to load a grids `./data/data-grid/grid_raster.tif` or `./data/data-grid/grid.poly.geojson`.
#' @param name name of column for the data imported in study grid, defaults to "intensity"
#' @param negative logical, if FALSE then values < 0 are filtered out
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
masteringrid <- function(dat, grd = here::here("data/grid/grid.tif")) {
  # stars objects
  if ("stars" %in% class(dat)) {
    # Get grid
    if (class(grd) == "character") grd <- stars::read_stars(grd)

    # Integrate in grid
    stars::st_warp(dat, grd)
  }
}
