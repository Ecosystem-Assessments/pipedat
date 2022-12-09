#' @eval get_name("72312316")
#'
#' @eval get_description("72312316")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param shipping_model one of "interpolated" or "noninterpolated"
#' @param shipping_type one of "num_vessels" or "hours"
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
di_72312316 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, shipping_model = "interpolated", shipping_type = "num_vessels", ...) {
  # Output folders and other objects used
  uid <- "72312316"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

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
    if (shipping_model == "interpolated") {
      shipping <- dat[["shipping_gfw-8449dee0-interpolated.csv"]]
    }

    if (shipping_model == "noninterpolated") {
      shipping <- dat[["shipping_gfw-8449dee0-noninterpolated.csv"]]
    }

    # # Study grid, if applicable
    # if (is.null(grid)) {
    #   # grid <- sf::st_read("data/data-grid/grid_poly.geojson", quiet = TRUE)
    #   grid <- stars::read_stars("data/data-grid/grid_raster.tif", quiet = TRUE)
    # }
    # grid <- sf::st_transform(grid, crs = 4326)
    # names(grid) <- "uid"
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
        masteringrid()    
      }

    # Devide hours and num_vessels
    n <- glue::glue("{nm}-{shipping_type}-{years}")
    ship_vessels <- ship_hours <- list()
    for (i in 1:length(ship)) {
      ship_vessels[[i]] <- dplyr::select(ship[[i]], num_vessels)
      ship_hours[[i]] <- dplyr::select(ship[[i]], hours)
      names(ship_vessels[[i]]) <- glue::glue("{n[i]}-num_vessels")
      names(ship_hours[[i]]) <- glue::glue("{n[i]}-hours")
    }
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "integration",
      pipeline_id = uid,
      integration_data = raw_id
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
    if (shipping_type == "num_vessels") {
      fm <- here::here(path, glue::glue("{nm}-{shipping_model}-vessels-{years}"))
      for (i in 1:length(years)) masterwrite(ship_vessels[[i]], fm[i])
    }
    if (shipping_type == "hours") {
      fm <- here::here(path, glue::glue("{nm}-{shipping_model}-hours-{years}.csv"))
      for (i in 1:length(years)) masterwrite(ship_hours[[i]], fm[i])
    }

    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)  
    # _________________________________________________________________________________________ #
  }
}
