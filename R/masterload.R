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

  # Import
  dat <- switch(ext, 
    "gpkg" = sf::st_read(path, quiet = TRUE),
    "geojson" = sf::st_read(path, quiet = TRUE),
    "shp" = sf::st_read(path, quiet = TRUE),
    "tif" = stars::read_stars(path, quiet = TRUE),
    "csv" = vroom::vroom(path)
  )

  # Identify which data were loaded
  msgInfo("Imported file:", glue::glue("{name}.{ext}"))

  # -----
  invisible(dat)
}

#' @name masterload
#' @export
masterwrite <- function(obj, path) {
  # Identify extension to use for object export
  ext <- make_extension(obj)

  # Export 
  switch(ext, 
    "gpkg" = sf::st_write(obj, glue::glue("{path}.{ext}"), quiet = TRUE, append = FALSE),
    "tif" = stars::write_stars(obj, glue::glue("{path}.{ext}"), quiet = TRUE),
    "csv" = vroom::vroom_write(obj, glue::glue("{path}.{ext}"), delim = ","),
    "yaml" = yaml::write_yaml(obj, glue::glue("{path}.{ext}"), column.major = FALSE),
    "bib" = RefManageR::WriteBib(obj, file = glue::glue("{path}.{ext}"), verbose = FALSE)
  )

  # Identify which data were loaded
  msgInfo("Exported file:", glue::glue("{basename(path)}.{ext}"))
}


#' @name make_extension
#' @export
make_extension <- function(obj) {
  cls <- class(obj)
  dplyr::case_when(
    "sf" %in% cls ~ "gpkg",
    "stars" %in% cls ~ "tif",
    (("matrix" %in% cls | "data.frame" %in% cls) & (!"stars" %in% cls & !"sf" %in% cls)) ~ "csv",
    "list" %in% cls ~ "yaml", 
    "BibEntry" %in% cls ~ "bib"
  )
}
