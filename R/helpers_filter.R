# ------------------------------------------------------------------------------
# Intersection with bounding box
bbox_crop <- function(dat, bbox, crs) {
  bb <- bbox_poly(bbox, crs)
  if ("sf" %in% class(dat)) {
    sf::st_intersection(dat, bb)
  } else if ("stars" %in% class(dat)) {
    sf::st_crop(dat, bb)
  } else if (class(dat) == "data.frame") {
    uid <- (dat$longitude >= bbox['xmin'] & dat$longitude <= bbox['xmax']) &
      (dat$latitude >= bbox['ymin'] & dat$latitude <= bbox['ymax'])
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
dp_parameters <- function(dat, crs = NULL, bbox = NULL, timespan = NULL) {
  if (!is.null(crs)) {
    dat <- sf::st_transform(dat, crs = crs)
  }

  if (!is.null(bbox)) {
    dat <- bbox_crop(dat, bbox, crs)
  }

  if (!is.null(timespan)) {
    dat <- timespan_filter(dat, timespan)
  }

  invisible(dat)
}
