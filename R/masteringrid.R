#' Generic function incorporate data in study grid
#'
#' This function is used to incorporate data in a study grid in a generic way, *e.g.* by
#' warping raster data to a new raster object with a different resolution.
#' user-specified study grid, if applicable, which we refer to as *data integration pipelines*.
#'
#' @param dat object with spatial data to import in study grid, either `sf` or `stars` object
#' @param grid spatial grid used for data integration. Can be a `sf` object containing polygons or a `stars` rasters that will be used as a template. Individual cells must be identified by a unique identifier called `uid`. If NULL, the function attempts to load a grids `./data/data-grid/grid_raster.tif` or `./data/data-grid/grid.poly.geojson`.
#' @param name name of column for the data imported in study grid, defaults to "intensity"
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
masteringrid <- function(dat, grid = NULL, name = "intensity") {
  # WARNING: For R CMD CHECK
  intensity <- uid <- x <- y <- NULL

  # stars objects
  if ("stars" %in% class(dat)) {
    # Get grid
    if (is.null(grid)) {
      grid <- stars::read_stars("data/data-grid/grid_raster.tif", quiet = TRUE)
      names(grid) <- "uid"
    }
    grid <- sf::st_transform(grid, st_crs(dat))

    # Integrate in grid
    dat <- stars::st_warp(dat, grid) |>
      c(grid) |>
      as.data.frame() |>
      dplyr::filter(!is.na(uid)) |>
      dplyr::arrange(uid) |>
      dplyr::select(-x, -y) |>
      stats::setNames(c("intensity", "uid")) |>
      dplyr::filter(intensity > 0) |>
      dplyr::select(uid, intensity)

    if (nrow(dat) > 0) colnames(dat)[2] <- name
    dat
  }

  # # sf objects
  # if ("sf" %in% class(dat)) {
  #   # Get grid
  #   if (is.null(grid)) {
  #     grid <- sf::st_read("data/data-grid/grid_poly.geojson", quiet = TRUE)
  #   }
  #   grid <- sf::st_transform(grid, st_crs(dat))
  # }
}
