#' Imports data in global R environment
#'
#' This function is used to import data of various format in R
#'
#' @param path path to disk of the file to import
#'
#' @return This function returns objects in the global R environment
#'
#' @export
#'
#' @examples
#' \dontrun{
#' masterload("path")
#' }
#' @export
masterload <- function(path) {
  # Identify extension of the file to import
  ext <- tools::file_ext(path)
  name <- tools::file_path_sans_ext(basename(path))

  # Import file depending on extension
  ## GEOJSON
  if (ext %in% c("geojson", "shp")) {
    dat <- sf::st_read(path, quiet = TRUE)
  }

  ## GeoTIFF
  if (ext == "tif") {
    dat <- stars::read_stars(path, quiet = TRUE)
  }

  ## CSV
  if (ext == "csv") {
    dat <- utils::read.csv(path)
  }

  # Identify which data were loaded
  msgInfo("Imported file:", glue("{name}.{ext}"))

  # -----
  invisible(dat)
}
