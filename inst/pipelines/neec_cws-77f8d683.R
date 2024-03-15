#' @eval get_name("77f8d683")
#'
#' @eval get_description("77f8d683")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 77f8d683
#'
#' @examples
#' \dontrun{
#' dp_77f8d683()
#' }
dp_77f8d683 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, ...) {
  uid <- "77f8d683"
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
    asset <- "NEEC/"
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
      dir(full.names = TRUE, recursive = TRUE, pattern = ".csv")

    # NEEC
    neec <- masterload(here::here(path, "raw", "neec.csv")) |>
      dplyr::rename(
        incident_number = `Incident Number`,
        incident_name = `Incident Name`,
        reported_by = `Reported by to NEEC`,
        incident_date_time = `Incident Local Date Time (Local time)`,
        incident_unknown_date = `Unknown Incident Date`,
        notification_date_time = `Notification Date Time`,
        province = Province,
        latitude = Latitude,
        longitude = Longitude,
        coordinates_precision = `Coordinates Precision`,
        coordinates_verified = `Coordinates Verified by GISUL`,
        incident_category = `Incident Category`,
        impact_waterbody = `Impact to Waterbody`,
        spill_source = `Spill Source2`,
        waterbody = `Waterbody2`,
        distance_to_waterbody = `Distance to Waterbody`,
        unknown_distance_waterbody = `Unknown distance to closest waterbody`,
        reported_impacts = `Reported Impacts`,
        activation_duty_officer = `Activation of NEEC Duty Officer`,
        activation_triggers = `NEEC DO Activation Triggers`,
        notification_stakeholders = `Notification to Stakeholders`,
        act_regulation = `Act/Regulation`,
        substance_id = `Substance_Entity_ID`,
        substance = `Substance`,
        quantity = `Quantity_Converted`,
        unit = `Unit_Converted`,
        quantity_type = `Quantity Type`,
        freshwater_impact = `Fresh Water Impact`,
        marine_water_impact = `Marine Water Impact`,
        notification_cws = `Notification to CWS`
      ) |>
      dplyr::filter(!dplyr::if_all(dplyr::everything(), ~ is.na(.x)))

    # Taxonomy
    taxonomy <- masterload(here::here(path, "raw", "neec_taxonomy.csv")) |>
      dplyr::rename(
        vocabulary = Vocabulary,
        term_en = `Term EN`,
        term_fr = `Term FR`
      )

    # Subset data (if specified by user)
    # neec <- dp_parameters(neec, timespan = timespan)

    # Export
    fm <- here::here(path, "format", glue::glue("{nm}{c('','-taxonomy')}"))
    masterwrite(neec, fm[1])
    masterwrite(taxonomy, fm[2])

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
