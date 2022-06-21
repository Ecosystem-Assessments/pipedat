#' Function for basic plots
#'
#' These functions are used to produce basic plots of the grid and data integrated using
#' the pipedat package, and used to produce a summary report using `pipereport()`.
#'
#' @param uid unique identifier of integrated data to plot
#' @param grid sf object of the grid used for data integration. If NULL, the function attempts to
#' import `./data/data-grid/grid_poly.geojson`.
#'
#' @family figure functions
#' @rdname plotdat
#'
#' @return Exports pngs of the figures selected
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plotdat()
#' }
plotdat <- function(uid) {
  # Data
  dat <- griddat(uid) |>
    dplyr::select(-uid)
  nm <- colnames(sf::st_drop_geometry(dat))
  name <- gsub("-", " - ", nm)
  name <- gsub("_", " ", name)
  name <- stringr::str_to_sentence(name)
  bbox <- sf::st_bbox(dat)

  # Folders
  path <- here::here("figures", "figures-integrated")
  exist <- file.exists(path)
  if (!exist) dir.create(path, recursive = TRUE)

  for (i in 1:length(nm)) {
    grDevices::png(
      here::here(path, glue("{nm[i]}.png")),
      res = 300,
      width = 100,
      height = 70,
      units = "mm",
      pointsize = 12
    )
    pipeplot(
      dat[, i],
      bbox = bbox,
      main = name[i]
    )
    grDevices::dev.off()
  }
}

#' @rdname plotdat
#' @export
plotgrid <- function(grid = NULL) {
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
