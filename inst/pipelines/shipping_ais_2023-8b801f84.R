#' @eval get_name("8b801f84")
#'
#' @eval get_description("8b801f84")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 8b801f84
#'
#' @examples
#' \dontrun{
#' dp_8b801f84()
#' }
dp_8b801f84 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, ...) {
  uid <- "8b801f84"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_raw(uid)) {
    # Data loaded from private Google Cloud Storage bucket
    # Authentication
    .gcs_auth <- dir(pattern = "pof-stac-insileco")
    googleCloudStorageR::gcs_auth(.gcs_auth)
    Sys.setenv("GCS_AUTH_FILE" = .gcs_auth)

    # Bucket
    bucket <- "cws-private"
    asset <- "AIS/"
    assets_list <- googleCloudStorageR::gcs_list_objects(bucket = bucket) |>
      dplyr::filter(stringr::str_detect(name, asset))

    # Download
    lapply(assets_list$name, function(x) {
      googleCloudStorageR::gcs_get_object(
        x,
        bucket = bucket,
        saveToDisk = here::here(path, "raw", basename(x))
      )
    })


    # ~~~~~~~~~~~~~~~~~~~ #
    # Metadata
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox,
      pipeline_timespan = timespan,
      access = timestamp()
    )

    # bibtex
    bib <- get_bib(uid)

    # Export
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)
    write_pipeline(uid)
  }
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_format(uid)) {
    # Files
    files <- here::here(path, "raw") |>
      dir(full.names = TRUE, recursive = TRUE)

    # Import
    dat <- masterload(files)

    # Format data
    # WARNING: In order for filters to work, names of column should be:
    # year = year
    # longitude = longitude
    # latitude  = latitude

    # Subset data (if specified by user)
    # on.exit(sf::sf_use_s2(TRUE), add = TRUE)
    # sf::sf_use_s2(FALSE)
    # dat <- lapply(dat, dp_parameters, bbox = bbox, timespan = timespan)
    dat <- dp_parameters(dat, bbox, timespan)

    # Export
    fm <- here::here(path, "format", glue::glue("{nm}"))
    masterwrite(dat, fm)

    # ~~~~~~~~~~~~~~~~~~~ #
    meta <- load_metadata(path, nm) |>
      add_format(
        format = list(
          timestamp = timestamp(),
          description = "",
          filenames = ""
        )
      )
    masterwrite(meta, here::here(path, nm))
  }
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Integrate data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_ingrid(uid) & ingrid) {
    dat <- importdat(uid, "format")[[1]] |>
      stars::st_rasterize() |>
      masteringrid()

    # Export
    masterwrite(dat, here::here(path, "ingrid", nm))

    # ~~~~~~~~~~~~~~~~~~~ #
    meta <- add_ingrid(meta,
      ingrid = list(
        timestamp = timestamp(),
        description = "",
        files = list(
          filenames = nm,
          names = "" # For report
        )
      )
    )
    masterwrite(meta, here::here(path, nm))
  }
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Additional metadata if applicable
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # To add additional metadata for queried data
  meta <- add_metadata(meta,
    info1 = c("Format as lists and dataframes to be rendered as yaml"),
    info2 = c("Formatting thus matters"),
    info3 = c("Go to https://github.com/vubiostat/r-yaml for more information")
  )

  # Clean
  clean_path(uid)
  # _________________________________________________________________________________________ #
}
