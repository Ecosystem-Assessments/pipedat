#' @eval get_name("04528545")
#'
#' @eval get_description("04528545")
#'
#' @eval dp_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 04528545
#'
#' @examples
#' \dontrun{
#' dp_04528545()
#' }
dp_04528545 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "04528545"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!exist$raw) {
    govcan <- get_pipeline(uid)$data_uuid
    pipeload(
      govcan = govcan,
      output = here::here(path, "raw"),
      large = TRUE
    )
  }
  # _________________________________________________________________________________________ #

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    dat <- sf::st_read(
      here::here(path, "raw", "Distribution_FGP.gdb"),
      layer = "DFO_SARA_Dist_2022_FGP_EN",
      quiet = TRUE
    )

    layers <- sf::st_layers(here::here(path, "raw", "Distribution_FGP.gdb"))
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # FORMAT DATA
    # NOTE: optional
    # WARNING: In order for filters to work, names of column should be:
    #             year      = year
    #             longitude = longitude
    #             latitude  = latitude
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox,
      pipeline_bbox_crs = bbox_crs,
      pipeline_timespan = timespan,
      access = timestamp(),
      data_bbox = sf::st_bbox(dat),
      data_timespan = sort(unique(dat$year))
    )

    # To add additional metadata for queried data
    meta <- add_metadata(meta,
      info1 = c("Format as lists and dataframes to be rendered as yaml"),
      info2 = c("Formatting thus matters"),
      info3 = c("Go to https://github.com/vubiostat/r-yaml for more information")
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
    dat <- dp_parameters(
      dat,
      bbox = bbox,
      bbox_crs = bbox_crs,
      timespan = timespan
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    fm <- here::here(path, glue("{nm}.geojson")) # NOTE: not necessarily spatial data
    sf::st_write(dat, dsn = fm, quiet = TRUE) # for spatial data

    # Metadata
    mt <- here::here(path, glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
