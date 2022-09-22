#' Imports and export data in or from global R environment
#'
#' These functions are used to import or export data of various format in R
#'
#' @param path path to disk of the file to import or export. Do not specify the extension if exporting using `masterwrite()`.
#' @param obj object to write to disk
#'
#' @return This function returns objects in the global R environment
#'
#' @name masterload
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

  ## CSV or DAT
  if (ext == "csv") {
    dat <- utils::read.csv(path)
  }

  # Identify which data were loaded
  msgInfo("Imported file:", glue::glue("{name}.{ext}"))

  # -----
  invisible(dat)
}

#' @name masterload
#' @export
masterwrite <- function(obj, path) {
  # Identify class of object
  cls <- class(obj)

  # Import file depending on extension
  ## GEOJSON
  if ("sf" %in% cls) {
    ext <- "geojson"
    sf::st_write(obj, glue::glue("{path}.{ext}"), quiet = TRUE)
  }

  ## GeoTIFF
  if ("stars" %in% cls) {
    ext <- "tif"
    stars::write_stars(obj, glue::glue("{path}.{ext}"), quiet = TRUE)
  }

  ## CSV
  if (
    ("matrix" %in% cls | "data.frame" %in% cls) &
      (!"stars" %in% cls & !"sf" %in% cls)
  ) {
    ext <- "csv"
    utils::write.csv(obj, glue::glue("{path}.{ext}"), row.names = FALSE)
  }

  ## YAML
  if ("list" %in% cls) {
    ext <- "yaml"
    yaml::write_yaml(obj, glue::glue("{path}.{ext}"), column.major = FALSE)
  }

  ## Bibtex
  if ("BibEntry" %in% cls) {
    ext <- "bib"
    RefManageR::WriteBib(obj, file = glue::glue("{path}.{ext}"), verbose = FALSE)
  }

  # Identify which data were loaded
  msgInfo("Exported file:", glue::glue("{basename(path)}.{ext}"))
}
