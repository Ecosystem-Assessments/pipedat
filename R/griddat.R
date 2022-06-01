#' Create spatial objects from grid and integrated data
#'
#' This function is used to join integrated data with study grid
#'
#' @param uid unique identifiers of queried datasets. The full list of available data pipelines can be consulted using `pipelines()`
#' @param grid spatial grid used for data integration. Can be a `sf` object containing polygons or a `stars` rasters that will be used as a template. Individual cells must be identified by a unique identifier called `uid`. If NULL, the function attempts to load a grid `./data/data-grid/grid.poly.geojson`.
#'
#' @return This function returns objects in the global R environment
#'
#' @export
#'
#' @examples
#' \dontrun{
#' importdat("f635934a")
#' }
#' @export
griddat <- function(uid, grid = NULL) {
  dat <- importdat(uid)
  nm <- tools::file_path_sans_ext(names(dat))
  for (i in 1:length(dat)) {
    dat[[i]] <- setNames(
      dat[[i]],
      c("uid", nm[i])
    )
  }

  # Grid
  if (is.null(grid)) {
    grid <- sf::st_read("data/data-grid/grid_poly.geojson", quiet = TRUE)
  }

  # Join all data together
  dat <- purrr::reduce(dat, dplyr::left_join, by = "uid")

  # Join to grid
  dat <- dplyr::left_join(grid, dat, by = "uid")

  # return
  invisible(dat)
}
