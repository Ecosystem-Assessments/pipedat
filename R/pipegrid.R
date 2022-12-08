#' Creates a raster study grid
#'
#' This function is used to create a raster study grid that will serve as a template for data extractions
#'
#' @eval grid_params()
#'
#' @return a tif file and a csv file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cellsize <- 1
#' crs <- 4326 
#' bb <- c(xmin = -45, ymin = -45, xmax = 45, ymax = 45)
#' bbox <- sf::st_bbox(bb, crs = crs)
#' aoi <- sf::st_as_sfc(bbox, crs = crs)
#' pipegrid(bb, cellsize, crs)
#' pipegrid(bbox, cellsize, crs)
#' pipegrid(aoi, cellsize, crs)
#' }
pipegrid <- function(aoi, cellsize, crs = 4326) {
  # Create grid template
  if ("sfc" %in% class(aoi)) {
    aoi <- sf::st_as_sf(aoi, crs = crs)
  } else if (class(aoi) %in% c("bbox","numeric","integer")){
    aoi <- sf::st_bbox(aoi, crs = crs) |>
           sf::st_as_sfc() |>
           sf::st_as_sf()
  }
  grd <- stars::st_rasterize(aoi, dx = cellsize, dy = cellsize)
  
  # Export
  ## Raster grid
  out <- here::here("data","grid")
  chk_create(out)
  stars::write_stars(grd, dsn = here::here(out, "grid.tif"), quiet = TRUE, delete_dsn = TRUE)
  
  ## Polygon of area of interest 
  out <- here::here("data","aoi")
  chk_create(out)
  sf::st_write(aoi, dsn = here::here(out, "aoi.gpkg"), quiet = TRUE, delete_dsn = TRUE)  
}