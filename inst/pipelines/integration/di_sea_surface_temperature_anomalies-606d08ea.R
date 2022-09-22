#' @eval get_name("606d08ea")
#'
#' @eval get_description("606d08ea")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 606d08ea
#'
#' @examples
#' \dontrun{
#' di_606d08ea()
#' }
di_606d08ea <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, ...) {
  # Output folders and other objects used
  uid <- "606d08ea"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

  if (!exist$integrated) {
    # WARNING: For R CMD CHECK
    dist <- grid_raster.tif <- intensity <- latitude <-
      longitude <- month <- sst_negative <- sst_positive <-
      val <- y <- NULL
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    raw_id <- get_rawid(uid) # String with data to import
    pipedat(raw_id, bbox, bbox_crs, timespan)
    dat <- importdat(raw_id)
    sst <- dat[["sea_surface_temperature-d87b7f5f.csv"]]
    ref <- dat[["sea_surface_temperature_climatology-1b4dba19.csv"]]

    # Study grid, if applicable
    if (is.null(grid)) {
      grid <- stars::read_stars("data/data-grid/grid_raster.tif", quiet = TRUE)
    }
    if (sf::st_crs(grid)$epsg != 4326) {
      grid <- sf::st_transform(grid, crs = 4326)
    }
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ANALYZE / FORMAT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                        EXTENT REFERENCE vs CURRENT
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Identify observations in current datasets (2013-2017) that overlap with reference (1980-2010)
    iid <- (sst$longitude %in% ref$longitude) & (sst$latitude %in% ref$latitude)

    # Subset sst
    # NOTE: this may not be useful later on when I have the full dataset
    sst <- sst[iid, ]

    # Join ref data to sst coordinates
    ref <- unique(sst[, c("longitude", "latitude")]) |>
      dplyr::left_join(ref, by = c("longitude", "latitude"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                          GAP-FILLING MISSING DATA
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Gap-fill missing data with mean of neighbouring values for each month
    # Takes > 10000s to run, i.e. ~ 3h
    idNA <- which(is.na(ref), arr.ind = TRUE) |>
      as.data.frame() |>
      dplyr::mutate(val = TRUE, month = colnames(ref)[col]) |>
      tidyr::pivot_wider(id_cols = row, names_from = month, values_from = val)

    temp <- sf::st_as_sf(ref, coords = c("longitude", "latitude"), crs = 4326)
    ref$id <- 1:nrow(ref)
    cnames <- colnames(temp)[-ncol(temp)]

    # Function to gap-fill missing data using a weighted average as a function of distance
    # with the four closest points used for calculations
    weighted_average <- function(dat) {
      dat <- stats::na.omit(dat)
      dat <- dat[1:4, ]
      sum(dat$val * (min(dat$dist) / dat$dist) / sum(min(dat$dist) / dat$dist))
    }

    for (i in 1:nrow(idNA)) {
      # Distance between point j and all other points
      df <- sf::st_distance(temp[idNA$row[i], ], temp) |>
        t() |>
        as.data.frame() |>
        stats::setNames("dist") |>
        dplyr::mutate(id = 1:dplyr::n()) |>
        dplyr::arrange(dist) |>
        dplyr::left_join(ref, by = "id")

      for (j in 1:length(cnames)) {
        x <- df[, c("dist", cnames[j])] |>
          stats::setNames(c("dist", "val"))
        ref[idNA$row[i], cnames[j]] <- ifelse(
          is.na(x$val[1]),
          weighted_average(x),
          x$val[1]
        )
      }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                             NORMALIZE ANOMALIES
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Normalize anomalies
    ref <- dplyr::slice(ref, rep(dplyr::row_number(), length(unique(sst$year))))
    for (i in cnames) {
      sst[, i] <- sst[, i] / ref[, i]
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                            POSITIVE & NEGATIVE ANOMALIES
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Transform values between -.5 and +.5 to NA
    trans <- function(x) ifelse(x > -.5 & x < .5, NA, x)
    for (i in cnames) {
      sst[, i] <- trans(sst[, i])
    }

    # Positive & negative anomalies
    pos <- neg <- sst
    for (i in cnames) {
      pos[, i] <- ifelse(pos[, i] < 0, NA, pos[, i])
      neg[, i] <- ifelse(neg[, i] > 0, NA, neg[, i])
    }

    # Transform negative anomalies as positive values
    for (i in cnames) {
      neg[, i] <- abs(neg[, i])
    }

    # Annual sum
    # NOTE: this part should be removed for monthly rather than annual rasters
    pos <- dplyr::mutate(pos, sst_positive = rowSums(pos[, cnames], na.rm = TRUE)) |>
      dplyr::select(longitude, latitude, year, sst_positive)
    neg <- dplyr::mutate(neg, sst_negative = rowSums(neg[, cnames], na.rm = TRUE)) |>
      dplyr::select(longitude, latitude, year, sst_negative)



    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                                 RASTERIZE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pipedat_rasterize <- function(pts) {
      dat <- stars::st_rasterize(pts, dx = 2000, dy = 2000) |>
        stars::st_warp(grid) |>
        c(grid) |>
        as.data.frame() |>
        dplyr::filter(!is.na(grid_raster.tif)) |>
        dplyr::arrange(uid) |>
        dplyr::select(-x, -y) |>
        stats::setNames(c("intensity", "uid")) |>
        dplyr::filter(intensity > 0) |>
        dplyr::select(uid, intensity)
    }

    # Spatial objects
    pos <- sf::st_as_sf(pos, coords = c("longitude", "latitude"), crs = 4326) |>
      sf::st_transform(crs = 32198)
    neg <- sf::st_as_sf(neg, coords = c("longitude", "latitude"), crs = 4326) |>
      sf::st_transform(crs = 32198)

    # Rasterize and warp to study area grid
    year <- sort(unique(sst$year))
    sst_pos <- sst_neg <- list()
    for (i in 1:length(year)) {
      iid <- pos$year == year[i]
      sst_pos[[i]] <- pipedat_rasterize(pos[iid, "sst_positive"])
      sst_neg[[i]] <- pipedat_rasterize(neg[iid, "sst_negative"])
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
    fm1 <- here::here(path, glue("{nm}-positive-{year}.csv"))
    fm2 <- here::here(path, glue("{nm}-negative-{year}.csv"))
    for (i in 1:length(year)) {
      utils::write.csv(sst_pos[[i]], fm1[i], row.names = FALSE)
      utils::write.csv(sst_neg[[i]], fm2[i], row.names = FALSE)
    }

    # Metadata
    mt <- here::here(path, glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  }
}
