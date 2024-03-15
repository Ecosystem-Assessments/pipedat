#' @eval get_name("376f0891")
#'
#' @eval get_description("376f0891")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 376f0891
#'
#' @examples
#' \dontrun{
#' dp_376f0891()
#' }
dp_376f0891 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, ...) {
  uid <- "376f0891"
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
    asset <- "NASP/"
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
    dat <- masterload(here::here(path, "raw", "nasp.csv"))

    # Apply formatting to make data usable
    colnames(dat) <- tolower(colnames(dat))
    dat <- dat |>
      dplyr::filter(!is.na(latitude)) |>
      unique() |>
      dplyr::mutate(date = lubridate::dmy(date))

    # Transform coordinates and create spatial object
    format_coord <- function(x, type = "latitude") {
      # Basic transformations
      x <- tolower(x) |>
        stringr::str_replace_all(" ", "") |>
        stringr::str_replace_all("w", "") |>
        stringr::str_replace_all("n", "") |>
        stringr::str_replace_all("'", "") |>
        stringr::str_replace_all('"', "") |>
        stringr::str_replace_all("/", "") |>
        stringr::str_replace_all(",", "\\.") |>
        stringr::str_replace_all(":", "") |>
        stringr::str_replace_all("\\.", "") |>
        stringr::str_replace_all(">", "")

      if (type == "latitude") {
        # Transform, the formula is: Dd = DD + MM.MM/60
        dd <- as.numeric(substr(x, 1, 2))
        mm <- as.numeric(glue::glue("{substr(x, 3, 4)}.{substr(x, 5, nchar(x))}")) / 60
        x <- dd + mm
      } else if (type == "longitude") {
        # Some values are > 100. Values < 100 thus all need to start with a 0
        iid <- as.numeric(substr(x, 1, 1)) > 1
        x[iid] <- glue::glue("0{x[iid]}")

        # Transform, the formula is: Dd = DD + MM.MM/60
        dd <- as.numeric(substr(x, 1, 3))
        mm <- as.numeric(glue::glue("{substr(x, 4, 5)}.{substr(x, 6, nchar(x))}")) / 60
        x <- -(dd + mm)
      }

      x
    }

    dat <- dat |>
      dplyr::mutate(
        latitude = format_coord(latitude, "latitude"),
        longitude = format_coord(longitude, "longitude")
      ) |>
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

    # Export
    fm <- here::here(path, "format", glue::glue("{nm}"))
    masterwrite(dat, fm)

    # ~~~~~~~~~~~~~~~~~~~ #
    meta <- load_metadata(path, nm) |>
      add_format(
        format = list(
          timestamp = timestamp(),
          filenames = fm
        )
      )
    masterwrite(meta, here::here(path, nm))
  }
  # _________________________________________________________________________________________ #

  # Clean
  clean_path(uid)
  # _________________________________________________________________________________________ #
}
