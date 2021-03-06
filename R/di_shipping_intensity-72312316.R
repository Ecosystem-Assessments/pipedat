#' @eval get_name("72312316")
#'
#' @eval get_description("72312316")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param shipping_type one of "interpolated" or "noninterpolated"
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 72312316
#'
#' @examples
#' \dontrun{
#' di_72312316()
#' }
di_72312316 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, shipping_type = "interpolated", ...) {
  # Output folders and other objects used
  uid <- "72312316"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  exist <- check_files(uid, name, ondisk = FALSE)
  path <- make_output(uid, name)

  if (!exist$integrated) {
    # WARNING: For R CMD CHECK
    hours <- latitude <- longitude <- month <- num_vessels <-
      vessel_class <- x <- y <- year <- NULL

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    raw_id <- get_rawid(uid) # String with data to import
    pipedat(raw_id, bbox, bbox_crs, timespan)
    dat <- importdat(raw_id)
    if (shipping_type == "interpolated") {
      shipping <- dat[["shipping_gfw-8449dee0-interpolated.csv"]]
    }

    if (shipping_type == "noninterpolated") {
      shipping <- dat[["shipping_gfw-8449dee0-noninterpolated.csv"]]
    }

    # Study grid, if applicable
    if (is.null(grid)) {
      # grid <- sf::st_read("data/data-grid/grid_poly.geojson", quiet = TRUE)
      grid <- stars::read_stars("data/data-grid/grid_raster.tif", quiet = TRUE)
    }
    grid <- sf::st_transform(grid, crs = 4326)
    names(grid) <- "uid"
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ANALYZE / FORMAT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # For this intensity measurement, combine all vessel classes
    shipping <- dplyr::select(shipping, -vessel_class, -month) |>
      dplyr::group_by(year, longitude, latitude) |>
      dplyr::summarize(
        num_vessels = sum(num_vessels),
        hours = sum(hours)
      ) |>
      dplyr::ungroup()
    # sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

    years <- sort(unique(shipping$year))
    ship <- list()
    for (i in 1:length(years)) {
      datid <- shipping$year == years[i]
      ship[[i]] <- shipping[datid, ] |>
        dplyr::select(-year) |>
        stars::st_as_stars(
          shipping[datid, ],
          coords = c("longitude", "latitude")
        ) |>
        sf::st_set_crs(4326) |>
        stars::st_warp(grid) |>
        c(grid) |>
        as.data.frame() |>
        dplyr::filter(!is.na(uid)) |>
        dplyr::arrange(uid) |>
        dplyr::select(-x, -y)
    }

    # Devide hours and num_vessels
    n <- glue("{nm}-{shipping_type}-{years}")
    ship_vessels <- ship_hours <- list()
    for (i in 1:length(ship)) {
      ship_vessels[[i]] <- dplyr::select(ship[[i]], uid, num_vessels)
      ship_hours[[i]] <- dplyr::select(ship[[i]], uid, hours)
      colnames(ship_vessels[[i]])[2] <- glue("{n[i]}-num_vessels")
      colnames(ship_hours[[i]])[2] <- glue("{n[i]}-hours")
    }
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "integration",
      pipeline_id = uid,
      integration_data = raw_id,
      integration_grid = get_grid_info(grid) # if applicable
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    fm <- here::here(path, glue("{nm}-{shipping_type}-{years}-num_vessels.csv"))
    for (i in 1:length(years)) utils::write.csv(ship_vessels[[i]], fm[i], row.names = FALSE)
    fm <- here::here(path, glue("{nm}-{shipping_type}-{years}-hours.csv"))
    for (i in 1:length(years)) utils::write.csv(ship_hours[[i]], fm[i], row.names = FALSE)

    # Metadata
    mt <- here::here(path, glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  }
}
