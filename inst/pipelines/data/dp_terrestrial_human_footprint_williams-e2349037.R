#' @eval get_name("e2349037")
#'
#' @eval get_description("e2349037")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: e2349037
#'
#' @examples
#' \dontrun{
#' dp_e2349037()
#' }
dp_e2349037 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "e2349037"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!exist$raw) {
    # If the data is downloaded from online sources
    urls <- c(
      "https://datadryad.org/stash/downloads/file_stream/402761",
      "https://datadryad.org/stash/downloads/file_stream/402868",
      "https://datadryad.org/stash/downloads/file_stream/402897",
      "https://datadryad.org/stash/downloads/file_stream/402762",
      "https://datadryad.org/stash/downloads/file_stream/402763"
    )

    # Load
    pipeload(
      urls = urls,
      output = here::here(path, "raw"),
      large = TRUE
    )

    # Rename files
    file.rename(
      here::here(path, "raw", "402761"),
      here::here(path, "raw", "human_footprint_grass_scripts-402761.zip")
    )
    file.rename(
      here::here(path, "raw", "402868"),
      here::here(path, "raw", "human_footprint_maps-402868.zip")
    )
    file.rename(
      here::here(path, "raw", "402897"),
      here::here(path, "raw", "human_footprint_pressure_layers-402897.zip")
    )
    file.rename(
      here::here(path, "raw", "402762"),
      here::here(path, "raw", "human_footprint_reclassification_files-402762.zip")
    )
    file.rename(
      here::here(path, "raw", "402763"),
      here::here(path, "raw", "williams_et_al_results-402763.xls")
    )
  }
  # _________________________________________________________________________________________ #

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Unzip
    zipfiles <- dir(here::here(path, "raw"), pattern = ".zip", full.names = TRUE)
    lapply(zipfiles, function(x) utils::unzip(x, exdir = here::here(path, "raw")))

    files <- dir(here::here(path, "raw", "Human_footprint_pressure_layers"), full.names = TRUE)
    pressures <- lapply(files, masterload)

    files <- dir(here::here(path, "raw", "Human_footprint_maps"), full.names = TRUE)
    footprint <- lapply(files, masterload)

    dat <- c(pressures, footprint)
    datnames <- lapply(dat, names) |>
      unlist() |>
      tools::file_path_sans_ext()
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox,
      pipeline_bbox_crs = bbox_crs,
      access = timestamp(),
      data_bbox = sf::st_bbox(dat[[1]])
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # APPLY SUBSETS SPECIFIED BY USER
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    dat <- lapply(dat, dp_parameters, bbox = bbox, bbox_crs = bbox_crs)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    fm <- here::here(path, glue("{nm}-{datnames}.tif"))
    for (i in 1:length(fm)) {
      stars::write_stars(dat[[i]], fm[i])
    }

    # Delete to save memory
    unlink(here::here(path, "raw", "Human_footprint_grass_scripts"), recursive = TRUE)
    unlink(here::here(path, "raw", "Human_footprint_maps"), recursive = TRUE)
    unlink(here::here(path, "raw", "Human_footprint_pressure_layers"), recursive = TRUE)
    unlink(here::here(path, "raw", "human_footprint_reclassification_files"), recursive = TRUE)

    # Metadata
    mt <- here::here(path, glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
