#' @eval get_name("9d64101c")
#'
#' @eval get_description("9d64101c")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 9d64101c
#'
#' @examples
#' \dontrun{
#' dp_9d64101c()
#' }
dp_9d64101c <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "9d64101c"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid, ondisk = TRUE)
  path <- make_output(uid)

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    utils::unzip(
      here::here(path, "raw", "SST.zip"),
      exdir = here::here(path, "raw", "SST")
    )
    files <- dir(here::here(path, "raw", "SST"))
    if (!is.null(timespan)) files <- files[as.numeric(files) %in% timespan]
    files <- dir(here::here(path, "raw", "SST", files), recursive = TRUE, full.names = TRUE)
    # There are potentially too many files, so I have to do everything in a loop to avoid
    # memory issues. I only load one to get the metadata and then I do everything else in a loop
    import_dat <- function(dat) {
      utils::read.table(dat) |>
        dplyr::rename(
          latitude = V1,
          longitude = V2,
          anomalies = V3,
          n_obs = V4
        )
    }
    dat <- import_dat(files[1])
    bb <- c(
      xmin = min(dat$longitude),
      ymin = min(dat$latitude),
      xmax = max(dat$longitude),
      ymax = max(dat$latitude)
    )
    # _________________________________________________________________________________________ #


    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox,
      pipeline_bbox_crs = bbox_crs,
      pipeline_timespan = timespan,
      access = timestamp(),
      data_bbox = bb,
      data_timespan = 2010:2022
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT METADATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # LOAD FILES, FORMAT, APPLY SUBSETS SPECIFIED BY USER & EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    for (i in 1:length(files)) {
      # Import file
      dat <- import_dat(files[i]) |>
        dp_parameters(bbox = bbox, bbox_crs = bbox_crs, data_crs = 4326)

      # Export formatted data
      ym <- basename(files[i]) |>
        substr(1, 6)
      fm <- here::here(path, glue("{nm}-{ym}"))
      masterwrite(dat, fm)
    }
    # Remove unzipped data to avoid using memory unnecessarily
    unlink(here::here(path, "raw", "SST"), recursive = TRUE)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
