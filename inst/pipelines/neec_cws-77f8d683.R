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

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Integrate data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_ingrid(uid) & ingrid) {
    neec <- importdat(uid, "format")[["neec_cws-77f8d683.csv"]]

    # ----------------------------------------------------------------
    # Filter NEEC data based on specified constraints
    # Remove empty coordinates
    neec <- neec |>
      dplyr::filter(!is.na(latitude))

    # ----------------------------------------------------------------
    # Use data from “Notification Date Time” when not available from “Incident Local Date Time” field
    neec <- neec |>
      dplyr::mutate(
        datetime = ifelse(!is.na(incident_date_time), incident_date_time, notification_date_time),
        date = as.Date(datetime, format = "%Y-%m-%d %H:%M"),
        year = format(date, "%Y")
      )

    # ----------------------------------------------------------------
    # Remove records containing "covid", "istop", "sipps"
    neec <- neec |>
      dplyr::filter(!stringr::str_detect(tolower(incident_name), "covid")) |>
      dplyr::filter(!stringr::str_detect(tolower(incident_name), "istop")) |>
      dplyr::filter(!stringr::str_detect(tolower(incident_name), "sipps"))

    # ----------------------------------------------------------------
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # TO VERIFY
    # Set all NA quantities to 1
    neec <- neec |>
      dplyr::mutate(quantity = ifelse(is.na(quantity), 1, quantity))
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    # ----------------------------------------------------------------
    # Select greatest quantity for individual spills with multiple substances
    neec <- neec |>
      dplyr::group_by(incident_name) |>
      dplyr::filter(quantity == max(quantity, na.rm = TRUE)) |>
      dplyr::ungroup()

    # ----------------------------------------------------------------
    # Based on email exhange with Robert Ronconi on 2024-02-23
    # Remove Incident Name containing "MVI" or "furnace oil"
    # This is to remove a few dozen incidents that generally won't impact marine birds
    neec <- neec |>
      dplyr::filter(!stringr::str_detect(tolower(incident_name), "mvi")) |>
      dplyr::filter(!stringr::str_detect(tolower(incident_name), "furnace oil"))

    # ----------------------------------------------------------------
    # Based on email exhange with Robert Ronconi on 2024-02-23
    # Keep all incidents within 1km of the coastline
    # Coastal outline
    getdat <- function(country) {
      tmp <- here::here(path, "tmp")
      chk_create(tmp)
      raster::getData("GADM", country = country, level = 0, path = tmp) |>
        sf::st_as_sf() |>
        sf::st_simplify(dTolerance = 600, preserveTopology = FALSE) |>
        sf::st_make_valid()
    }
    can <- getdat("CAN")
    usa <- getdat("USA")
    terre <- sf::st_union(can, usa) |>
      sf::st_transform(32198) |> # To get units in meters
      smoothr::fill_holes(threshold = units::set_units(1000, km^2))

    # 1km buffer inland
    terre_buf <- sf::st_buffer(terre, -1000) |>
      sf::st_transform(4326)

    # Filter data spatially
    neec <- neec |>
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    iid <- sf::st_disjoint(terre_buf, neec) |> unlist()
    neec <- neec[iid, ]

    # ----------------------------------------------------------------
    # To explore:
    # Impact to Waterbody field.  Consider if we should treat “actual” vs. “potential” spills differently.  Data on “potential” spills only started being recorded in April 2021 onward (no differentiation before that).  Some “Potential” spills may report very large amounts of substance that was not actually spilled (e.g.  Incident Number NL-20200611-03045-20 was a disabled vessel with 400000 L of fuel on board, but nothing spilled).  These “Potential” spills still represent risk even if no true pollution occurred.

    # NOTE: There is a single NA for that column
    # neec <- neec |>
    #   dplyr::group_by(impact_waterbody)

    # ----------------------------------------------------------------
    # TO COMPLETE WITH PROPER METHODOLOGY
    # neec |>
    #   stars::st_rasterize() |>
    #   masteringrid()

    # # Export
    # masterwrite(dat, here::here(path, "ingrid", nm))

    # # ~~~~~~~~~~~~~~~~~~~ #
    # meta <- add_ingrid(meta,
    #   ingrid = list(
    #     timestamp = timestamp(),
    #     description = "",
    #     files = list(
    #       filenames = nm,
    #       names = "" # For report
    #     )
    #   )
    # )
    # masterwrite(meta, here::here(path, nm))
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
