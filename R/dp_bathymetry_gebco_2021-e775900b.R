#' @eval get_name("e775900b")
#'
#' @eval get_description("e775900b")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: e775900b
#'
#' @examples
#' \dontrun{
#' dp_e775900b()
#' }
dp_e775900b <- function(crs = 4326, bbox = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "e775900b"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  exist <- check_files(uid, name, ondisk = FALSE)
  path <- make_output(uid, name)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!exist$raw) {
    # If the data is downloaded from online sources
    urls <- c(
      "https://www.bodc.ac.uk/data/open_download/gebco/gebco_2021_sub_ice_topo/geotiff/"
    )

    # Load
    pipeload(
      urls = urls,
      output = here::here(path, "raw"),
      large = TRUE
    )

    # Rename
    file.rename(
      here::here(path, "raw", "geotiff"),
      here::here(path, "raw", "geotiff.zip")
    )
  }
  # _________________________________________________________________________________________ #

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Unzip
    archive::archive_extract(
      here::here(path, "raw", "geotiff.zip"),
      dir = here::here(path, "raw"),
      files = c("GEBCO_2021_Grid.pdf", "GEBCO_Grid_terms_of_use.pdf")
    )
    archive::archive_extract(
      here::here(path, "raw", "geotiff.zip"),
      dir = here::here(path, "raw", "gebco_2021")
    )

    # Load all
    files <- dir(here::here(path, "raw", "gebco_2021"), pattern = ".tif$")
    dat <- lapply(
      here::here(path, "raw", "gebco_2021", files),
      stars::read_stars,
      proxy = TRUE
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    dat_bbox <- c(
      xmin = -180,
      ymin = -90,
      xmax = 180,
      ymax = 90
    )

    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox,
      access = timestamp(),
      data_bbox = dat_bbox,
      data_timespan = 2021
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
    dat <- lapply(dat, dp_parameters, crs = 4326, bbox = bbox)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    name <- gsub("\\.0", "", files)
    name <- gsub("gebco_2021_sub_ice_topo_", "", name) |>
      tools::file_path_sans_ext()
    fm <- here::here(path, glue("{nm}-{name}.tif"))
    for (i in 1:length(fm)) {
      suppressWarnings(
        try(stars::write_stars(dat[[i]], fm[i]), silent = TRUE)
      )
    }

    # Delete to save memory
    unlink(here::here(path, "raw", "gebco_2021"), recursive = TRUE)

    # Metadata
    mt <- here::here(path, glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
