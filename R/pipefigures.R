#' Function for basic plots
#'
#' These functions are used to produce basic plots of the grid and data integrated using
#' the pipedat package, and used to produce a summary report using `pipereport()`.
#'
#' @param grid sf object of the grid used for data integration. If NULL, the function attempts to
#' import `./data/data-grid/grid_poly.geojson`.
#'
#' @family figure functions
#' @rdname pipedat_figures
#'
#' @return Exports pngs of the figures selected
#'
#' @export
#'
#' @examples
#' \dontrun{
#' figure_grid()
#' }
figure_grid <- function(grid = NULL) {
  if (is.null(grid)) {
    grid <- sf::st_read("data/data-grid/grid_poly.geojson", quiet = TRUE)
  }
  grid <- sf::st_transform(grid, st_crs(basemap$can))

  # Folders
  path <- here::here("figures", "figures-grid")
  exist <- file.exists(path)
  if (!exist) dir.create(path, recursive = TRUE)

  grDevices::png(
    here::here(path, "grid.png"),
    res = 300,
    width = 225,
    height = 200,
    units = "mm"
  )
  graphics::par(mar = c(0, 0, 0, 0))
  plot(
    sf::st_geometry(basemap$can),
    col = "#575757DD",
    border = "#575757",
    lwd = .5
  )
  plot(
    sf::st_geometry(grid),
    add = TRUE,
    lwd = 0.1,
    col = "#24636533",
    border = "#246365"
  )
  grDevices::dev.off()
}
