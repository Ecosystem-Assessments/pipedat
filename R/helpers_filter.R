# ------------------------------------------------------------------------------
# Intersection with bounding box
bbox_crop <- function(dat, bbox, bbox_crs, data_crs = sf::st_crs(dat)) { 
  bb <- bbox_poly(bbox, bbox_crs) |>
        sf::st_transform(crs = data_crs)
  if ("sf" %in% class(dat)) {
    sf::st_intersection(dat, bb)
  } else if ("stars" %in% class(dat)) {
    sf::st_crop(dat, bb)
  } else if ("data.frame" %in% class(dat)) {
    xy <- sf::st_coordinates(bb)
    bbox <- c(
      xmin = min(xy[,'X']), 
      ymin = min(xy[,'Y']), 
      xmax = max(xy[,'X']), 
      ymax = max(xy[,'Y'])      
    )
    uid <- (dat$longitude >= bbox["xmin"] & dat$longitude <= bbox["xmax"]) &
      (dat$latitude >= bbox["ymin"] & dat$latitude <= bbox["ymax"])
    dat[uid, ]
  }
}

# Filter by year
# Years must be in column "year"
timespan_filter <- function(dat, timespan) {
  dat |>
    dplyr::filter((!!rlang::sym("year")) %in% timespan)
}

# Applying pipeline arguments set by user
dp_parameters <- function(dat, bbox = NULL, bbox_crs = NULL, data_crs = sf::st_crs(dat), timespan = NULL) {
  if (!is.null(bbox)) {
    dat <- bbox_crop(dat, bbox, bbox_crs, data_crs)
  }

  if (!is.null(timespan)) {
    dat <- timespan_filter(dat, timespan)
  }

  invisible(dat)
}
