#' @eval get_name("48ea8a05")
#'
#' @eval get_description("48ea8a05")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 48ea8a05
#'
#' @examples
#' \dontrun{
#' dp_48ea8a05()
#' }
dp_48ea8a05 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, ...) {
  uid <- "48ea8a05"
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
    asset <- "ISTOP/"
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
    # Unzip data
    c(
      "CIS_Oil_Events_GU_EC_MA_IE_2006_to_Jan6_2018.tzip",
      "CIS_Oil_Events_GU_EC_MA_IE_2018_after_Jan6.tzip",
      "CIS_Oil_Events_GU_EC_MA_IE_2019to2023.tzip"
    ) |>
      lapply(function(x) {
        archive::archive_extract(
          here::here(path, "raw", x),
          here::here(path, "format", "tmp")
        )
      })

    # Import
    dat <- here::here(path, "format", "tmp") |>
      dir(pattern = ".shp$", full.names = TRUE) |>
      lapply(masterload)

    # Format data
    mod_nm1 <- data.frame(
      from = c(
        "Img_Cat_Id",
        "Category",
        "Comments",
        "Slick_Deta",
        "Other_Deta",
        "ANOMDATE",
        "geometry"
      ),
      to = c(
        "slick_id",
        "category",
        "comments",
        "length_km",
        "details",
        "date",
        "geometry"
      )
    )

    mod_nm2 <- data.frame(
      from = c(
        "OILSLICKID",
        "CATEGORY",
        "LENGTH",
        "COMMENTS",
        "SLICKDETAI",
        "ANALYST",
        "MODDATE",
        "geometry"
      ),
      to = c(
        "slick_id",
        "category",
        "length_km",
        "comments",
        "details",
        "analyst",
        "date",
        "geometry"
      )
    )

    # Make modifs
    dat <- lapply(dat, \(x) {
      nm <- colnames(x)
      if ("Img_Cat_Id" %in% nm) {
        # Select and rename columns
        x <- x[, mod_nm1$from]
        colnames(x)[match(mod_nm1$from, colnames(x))] <- mod_nm1$to

        # Make length numeric
        x$length_km <- gsub(" km", "", x$length_km)
        x$length_km <- gsub(" long", "", x$length_km)
        x$length_km <- as.numeric(x$length_km)
      } else if ("OILSLICKID" %in% nm) {
        # Select and rename columns
        x <- x[, mod_nm2$from]
        colnames(x)[match(mod_nm2$from, colnames(x))] <- mod_nm2$to

        # Rename categories
        x$category <- gsub("1A - Slick attached to target / Nappe de pétrole attenant à une cible radar", "1A", x$category)
        x$category <- gsub("1B - Slick with taget in area / Nappe de pétrole avec la cible dans la zone", "1B", x$category)
        x$category <- gsub("2 - Slick without source / Nappe de pétrole sans source", "2", x$category)
        x$category <- gsub("3 - Possible oil / Possible Nappe de pétrole", "3", x$category)
      }
      x
    }) |>
      dplyr::bind_rows()

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

    # Remove unzipped files
    fs::dir_delete(here::here(path, "format", "tmp"))
  }
  # _________________________________________________________________________________________ #

  clean_path(uid)
  # _________________________________________________________________________________________ #
}
