#' Creates a study grid
#'
#' This function is used to create a study grid for data integration pipelines and for data workflows. It is a simple wrapper around `sf::st_make_grid`.
#'
#' @eval grid_params()
#'
#' @return soon described
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pipegrid()
#' }
pipegrid <- function(x = NULL, bbox = NULL, cellsize, crs = 4326) {
  if (!is.null(x) & !is.null(bbox)) {
    stop("Choose a single option between a `sf` or `sfc` object or a bounding box.")
  }

  # Offset by default is st_bbox(x)[c("xmin", "ymin")]. Will stick with that
  # Polygonal grid
  if (!is.null(x)) {
    grd_poly <- sf::st_make_grid(x, cellsize = cellsize, crs = crs)
  } else if (!is.null(bbox)) {
    grd_poly <- bbox_poly(bbox, crs) |>
      sf::st_make_grid(cellsize = cellsize, crs = crs)
  }

  # Raster grid
  grd_ras <- stars::st_as_stars(grd_poly, dx = cellsize, dy = cellsize)

  # Export
  out <- "data/data-grid/"
  if (!file.exists(out)) dir.create(out, recursive = TRUE)
  sf::st_write(grd_poly, dsn = glue::glue("{out}grid_poly.geojson"), quiet = TRUE)
  stars::write_stars(grd_ras, dsn = glue::glue("{out}grid_raster.tif"), quiet = TRUE)
}
