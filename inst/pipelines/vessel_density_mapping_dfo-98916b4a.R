#' @eval get_name("98916b4a")
#'
#' @eval get_description("98916b4a")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 98916b4a
#'
#' @examples
#' \dontrun{
#' dp_98916b4a()
#' }
dp_98916b4a <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, ...) {
  uid <- "98916b4a"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_raw(uid)) {
    # If the data is downloaded from online sources
    urls <- c(
      "https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/db28032a-aa9f-42e2-851e-c5ad2e890fe5/attachments/NorthwestAtlantic_VesselDensity_2013_AIS.zip",
      "https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/a3107ec8-b644-4d3a-b9db-7eafda8a54d3/attachments/NorthwestAtlantic_VesselDensity_2014_AIS.zip",
      "https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/9259a352-a977-4727-b2af-57dfe089de9c/attachments/NorthwestAtlantic_VesselDensity_2015_AIS.zip",
      "https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/6396e7b7-ce29-4d43-bca7-bd42b6534019/attachments/NorthwestAtlantic_VesselDensity_2016_AIS.zip",
      "https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/78d9bbad-c309-49d2-a09b-ebec48397876/attachments/NorthwestAtlantic_VesselDensity_2017_AIS.zip",
      "https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/6e7f4547-9b97-4d3d-93de-266e752e2315/attachments/NorthwestAtlantic_VesselDensity_2018_AIS.zip",
      "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Vessel_Density_Mapping_AIS_Data_Northwest_Atlantic/TIF_for_Download_Albers.zip",
      "https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/ec9d2b6d-19e5-4bd4-acc3-c26a36e9f522/attachments/NorthwestAtlantic_VesselDensity_2020_AIS.zip",
      "https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/1eac2356-b120-42d9-af69-82f03a650bc6/attachments/NorthwestAtlantic_VesselDensity_2021_AIS.zip",
      "https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/248e2e14-456c-414e-b093-14022d367fb0/attachments/NorthwestAtlantic_VesselDensity_2022_AIS.zip",
      "https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/41105163.pdf"
    )

    # Load
    pipeload(
      urls = urls,
      output = here::here(path, "raw"),
      large = TRUE
    )

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
      dir(full.names = TRUE, recursive = TRUE, pattern = "*.tif$")

    # Names
    name <- basename(files) |>
      tools::file_path_sans_ext() |>
      stringr::str_replace("_AIS", "") |>
      stringr::str_replace("VesselsPerDay_", "") |>
      stringr::str_split("_") |>
      lapply(function(x) {
        if (length(x) == 2) {
          y <- data.frame(vessel = x[1], year = x[2], nm = glue::glue("{x[1]}_{x[2]}"))
        } else {
          y <- data.frame(vessel = x[1], year = x[3], month = x[2], nm = glue::glue("{x[1]}_{x[3]}_{x[2]}"))
        }
      }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(month = substr(month, 1, 3)) |>
      dplyr::mutate(files = files) |>
      dplyr::arrange(vessel, month, year)

    # Subset data years if timespan is specified
    if (!is.null(timespan)) name <- dplyr::filter(name, year %in% timespan)

    # Import
    dat <- lapply(name$files, masterload)

    # Subset data (if specified by user)
    # on.exit(sf::sf_use_s2(TRUE), add = TRUE)
    # sf::sf_use_s2(FALSE)
    dat <- lapply(dat, dp_parameters, bbox = bbox, timespan = timespan)

    # Export
    fm <- here::here(path, "format", glue::glue("{nm}_{name$nm}"))
    for (i in seq_len(length(dat))) masterwrite(dat[[i]], fm[i])

    # ~~~~~~~~~~~~~~~~~~~ #
    meta <- load_metadata(path, nm) |>
      add_format(
        format = list(
          timestamp = timestamp(),
          vessel = name$vessel,
          month = name$month,
          year = name$year,
          filenames = name$files
        )
      )
    masterwrite(meta, here::here(path, nm))
  }
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Integrate data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_ingrid(uid) & ingrid) {
    dat <- importdat(uid, "format") |>
      masteringrid()

    # Export
    fm <- here::here(path, "ingrid", tools::file_path_sans_ext(names(dat)))
    for (i in seq_len(length(dat))) masterwrite(dat[[i]], fm[i])

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


  # Clean
  clean_path(uid)
  # _________________________________________________________________________________________ #
}
