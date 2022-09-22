#' @eval get_name("7c8c4da1")
#'
#' @eval get_description("7c8c4da1")
#'
#' @eval dp_params()
#' @param ... further arguments used in functions, if applicable.
#' @param invasive_model model to load, either `current` or `projected`
#' @param invasive_model_type type of model to load, either `model` or `stdev`
#' @param invasive_species `species` and/or `richness`
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 7c8c4da1
#'
#' @examples
#' \dontrun{
#' dp_7c8c4da1()
#' }
dp_7c8c4da1 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, invasive_model = c("current", "projected"), invasive_model_type = c("model", "stdev"), invasive_species = c("species", "richness"), ...) {
  # Output folders and other objects used
  uid <- "7c8c4da1"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!exist$raw) {
    govcan <- get_pipeline(uid)$data_uuid
    pipeload(
      govcan = govcan,
      output = here::here(path, "raw"),
      large = FALSE
    )
  }
  # _________________________________________________________________________________________ #

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # NOTE: optional
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Example for data that needs to be locally available
    files_atl <- dir(
      here::here(path, "raw", "AtlanticTiff"),
      pattern = ".tif$",
      full.names = TRUE
    )
    files_pac <- dir(
      here::here(path, "raw", "PacificTiff"),
      pattern = ".tif$",
      full.names = TRUE
    )

    # NOTE: this is ugly af, but it works...
    aid <- rep(FALSE, length(files_atl))
    pid <- rep(FALSE, length(files_pac))
    if ("current" %in% invasive_model & "model" %in% invasive_model_type) {
      if ("species" %in% invasive_species) {
        aid <- aid | stringr::str_detect(files_atl, "ClimatologicalModel")
        pid <- pid | stringr::str_detect(files_pac, "ClimatologicalModel")
      }
      if ("richness" %in% invasive_species) {
        aid <- aid | stringr::str_detect(files_atl, "Present_Richness")
        pid <- pid | stringr::str_detect(files_pac, "PresentRichness")
      }
    }
    if ("current" %in% invasive_model & "stdev" %in% invasive_model_type) {
      if ("species" %in% invasive_species) {
        aid <- aid | stringr::str_detect(files_atl, "ClimatologicalStdDev")
        pid <- pid | stringr::str_detect(files_pac, "ClimatologicalStdDev")
      }
      if ("richness" %in% invasive_species) {
        aid <- aid | stringr::str_detect(files_atl, "Richness_Present_StdDev")
        pid <- pid | stringr::str_detect(files_pac, "Richness_Present_stddev")
      }
    }
    if ("projected" %in% invasive_model & "model" %in% invasive_model_type) {
      if ("species" %in% invasive_species) {
        aid <- aid | stringr::str_detect(files_atl, "ProjectedModel_2075")
        pid <- pid | stringr::str_detect(files_pac, "ProjectedModel_2075")
      }
      if ("richness" %in% invasive_species) {
        aid <- aid | stringr::str_detect(files_atl, "Projected_Richness")
        pid <- pid | stringr::str_detect(files_pac, "ProjectedRichness")
      }
    }
    if ("projected" %in% invasive_model & "stdev" %in% invasive_model_type) {
      if ("species" %in% invasive_species) {
        aid <- aid | stringr::str_detect(files_atl, "ProjectedStdDev")
        pid <- pid | stringr::str_detect(files_pac, "ProjectedStdDev")
      }
      if ("richness" %in% invasive_species) {
        aid <- aid | stringr::str_detect(files_atl, "Richness_Projection_StdDev")
        pid <- pid | stringr::str_detect(files_pac, "Richness_Projected_stddev")
      }
    }
    files_atl <- files_atl[aid]
    files_pac <- files_pac[pid]

    # Import
    atl <- lapply(files_atl, stars::read_stars, proxy = TRUE)
    pac <- lapply(files_pac, stars::read_stars, proxy = TRUE)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    ba <- sf::st_bbox(atl[[1]])
    bp <- sf::st_bbox(pac[[1]])
    dat_bbox <- c(
      xmin = min(c(ba[1], bp[1])),
      ymin = min(c(ba[2], bp[2])),
      xmax = max(c(ba[3], bp[3])),
      ymax = max(c(ba[4], bp[4]))
    )

    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox,
      pipeline_bbox_crs = bbox_crs,
      access = timestamp(),
      data_bbox = dat_bbox,
      data_timespan = c(2020, 2075)
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # APPLY SUBSETS AND CRS SPECIFIED BY USER
    # NOTE: optional, only if applicable
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    atl <- lapply(atl, dp_parameters, bbox = bbox, bbox_crs = bbox_crs)
    pac <- lapply(pac, dp_parameters, bbox = bbox, bbox_crs = bbox_crs)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    name_atl <- glue::glue("{tools::file_path_sans_ext(basename(files_atl))}-Atlantic")
    name_pac <- glue::glue("{tools::file_path_sans_ext(basename(files_pac))}-Pacific")
    fm_atl <- here::here(path, glue::glue("{nm}-{name_atl}.tif"))
    fm_pac <- here::here(path, glue::glue("{nm}-{name_pac}.tif"))
    # Atlantic
    for (i in 1:length(fm_atl)) {
      try(
        {
          x <- stars::st_as_stars(atl[[i]])
          stars::write_stars(x, fm_atl[i])
        },
        silent = TRUE
      )
    }
    # Pacific
    for (i in 1:length(fm_pac)) {
      try(
        {
          x <- stars::st_as_stars(pac[[i]])
          stars::write_stars(x, fm_pac[i])
        },
        silent = TRUE
      )
    }

    # Metadata
    mt <- here::here(path, glue::glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue::glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
