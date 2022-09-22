#' Series of functions to process the data and integration pipelines
#'
#' @param uid unique identifier of queried data. 
#' @param ondisk logical, whether the data should already be present locally, in cases when data are not available remotely (e.g. protected by data sharing agreements)
#'
#' @export
#' @describeIn pipeline_setup create path strings to folders and files
make_paths <- function(uid) {
  paths <- list()
  name <- get_shortname(uid)

  # Output folder for clean data
  paths$clean_output <- here::here(
    "data",
    "data-raw",
    glue::glue("{name}-{uid}")
  )

  # Clean files
  paths$clean_files <- here::here(
    paths$clean_output,
    files_clean$filepaths[files_clean$pipeline_id %in% uid]
  )

  # Output folder for raw data
  paths$raw_output <- here::here(
    paths$clean_output,
    "raw"
  )

  # Raw files
  paths$raw_files <- here::here(
    paths$raw_output,
    files_raw$filepaths[files_raw$pipeline_id %in% uid]
  )

  # Output folder for integrated data
  paths$integrated_output <- here::here(
    "data",
    "data-integrated",
    glue::glue("{name}-{uid}")
  )

  # Integrated files
  paths$integrated_files <- here::here(
    paths$integrated_output,
    files_integrated$filepaths[files_integrated$pipeline_id %in% uid]
  )

  invisible(paths)
}


#' @export
#' @describeIn pipeline_setup check if data is already present and send warning if data is already present and was thus not downloaded, or stop process if data needs to be available locally
check_files <- function(uid, ondisk = FALSE) {
  # Create paths
  paths <- make_paths(uid)

  # Check if raw files exist
  exist <- list()
  if (length(paths$raw_files) > 0) {
    exist$raw <- lapply(paths$raw_files, file.exists) |>
      unlist() |>
      any()
  } else {
    exist$raw <- FALSE
  }

  # Check if cleaned files exist
  if (length(paths$clean_files) > 0) {
    exist$clean <- lapply(paths$clean_files, file.exists) |>
      unlist() |>
      any()
  } else {
    exist$clean <- FALSE
  }

  # Check if integrated files exist
  if (length(paths$integrated_files) > 0) {
    exist$integrated <- lapply(paths$integrated_files, file.exists) |>
      unlist() |>
      any()
  } else {
    exist$integrated <- FALSE
  }

  # Messages
  if (ondisk & !exist$raw) msgOnDisk(uid, paths) # If data is needed locally, stop process
  if (!ondisk & exist$raw) msgNoLoad(uid) # If data is downloaded, warning
  if (exist$clean) msgNoClean(uid) # If data is downloaded, warning
  if (exist$integrated) msgNoIntegration(uid) # If data is downloaded, warning

  invisible(exist)
}

#' @export
#' @describeIn pipeline_setup check if required folders exist
check_folders <- function(uid) {
  paths <- make_paths(uid)
  out <- list()
  out$raw <- file.exists(paths$raw_output)
  out$integrated <- file.exists(paths$integrated_output)
  invisible(out)
}

#' @export
#' @describeIn pipeline_setup create required folders
# Create output folders for data pipelines
make_output <- function(uid) {
  paths <- make_paths(uid)
  fold <- check_folders(uid)

  # Create output folders
  type <- pipeline$pipeline_type[pipeline$pipeline_id == uid]
  if (!fold$raw & type == "data") {
    dir.create(paths$raw_output, recursive = TRUE)
  }
  if (!fold$integrated & type == "integration") {
    dir.create(paths$integrated_output, recursive = TRUE)
  }

  # Create path to raw or integrated data
  if (type == "data") {
    path <- paths$clean_output
  }

  if (type == "integration") {
    path <- paths$integrated_output
  }

  invisible(path)
}


#' @describeIn pipeline_setup get pipeline information
#' @export
get_pipeline <- function(uid) {
  dat <- pipeline
  pipid <- numeric(length(uid))
  for (i in 1:length(uid)) {
    pipid[i] <- which(dat$pipeline_id == uid[i])
  }
  dat[pipid, ]
}

#' @describeIn pipeline_setup get pipeline shortname
#' @export
get_shortname <- function(uid) {
  dat <- get_pipeline(uid)
  dat$data_shortname
}

#' @describeIn pipeline_setup get pipeline name
#' @export
get_name <- function(uid) {
  dat <- get_pipeline(uid)
  dat$data_name
}

#' @describeIn pipeline_setup get pipeline description
#' @export
get_description <- function(uid) {
  dat <- get_pipeline(uid)
  dat$data_description
}

#' @describeIn pipeline_setup get pipeline contacts
#' @export
get_contact <- function(uid) {
  dat <- pcontact
  pipid <- dat$pipeline_id %in% uid
  iid <- dat$contact_id[pipid]
  contact[contact$contact_id %in% iid, ]
}

#' @describeIn pipeline_setup get pipeline creator
#' @export
get_creator <- function(uid) {
  dat <- pcreator
  pipid <- dat$pipeline_id %in% uid
  iid <- dat$contact_id[pipid]
  contact[contact$contact_id %in% iid, ]
}

#' @describeIn pipeline_setup get pipeline bibtex citekey
#' @export
get_citekey <- function(uid) {
  dat <- pcite
  pipid <- dat$pipeline_id %in% uid
  dat$citekey[pipid]
}

#' @describeIn pipeline_setup get pipeline bibtex information
#' @export
get_bib <- function(uid) {
  iid <- get_citekey(uid)
  bib[[bib$key %in% iid]]
}

#' @describeIn pipeline_setup get pipeline url
#' @export
get_pipeline_url <- function(uid) {
  dat <- get_pipeline(uid)
  repo <- "https://github.com/Ecosystem-Assessments/pipedat"
  glue::glue("{repo}/blob/main/R/dp_{dat$data_shortname}-{uid}.R")
}

#' @describeIn pipeline_setup get pipeline type
#' @export
get_pipeline_type <- function(uid) {
  dat <- lapply(uid, get_pipeline) |>
    dplyr::bind_rows()
  dat$pipeline_type
}

#' @describeIn pipeline_setup get raw data used for integration pipeline
#' @export
get_rawid <- function(uid) {
  dat <- integ
  pipid <- dat$integration_id %in% uid
  dat$data_id[pipid]
}

#' @describeIn pipeline_setup get folder path to data
#' @export
get_folderpaths <- function(uid) {
  dat <- get_pipeline(uid)
  here::here(
    "data",
    ifelse(dat$pipeline_type == "data", "data-raw", "data-integrated"),
    glue::glue("{dat$data_shortname}-{dat$pipeline_id}")
  )
}

#' @describeIn pipeline_setup create string of filepaths
#' @export
make_filepaths <- function(uid) {
  folders <- get_folderpaths(uid)
  l <- list()
  for (i in 1:length(uid)) {
    type <- get_pipeline(uid[i])$pipeline_type
    if (type == "data") dat <- files_clean
    if (type == "integration") dat <- files_integrated
    l[[i]] <- here::here(
      folders[i],
      dat$filepaths[dat$pipeline_id == uid[i]]
    )
  }
  unlist(l)
}

#' @describeIn pipeline_setup get path to files of a pipeline
#' @export
get_filepaths <- function(uid) {
  dat <- make_filepaths(uid)
  dat[file.exists(dat)]
}

#' @describeIn pipeline_setup get information on a grid
#' @export
get_grid_info <- function(grd = NULL) {
  if (is.null(grd)) {
    grd <- stars::read_stars("data/data-grid/grid_raster.tif", quiet = TRUE)
    names(grd) <- "uid"
  }

  type <- class(grd)
  poly <- "sf" %in% type
  type_text <- ifelse(poly, "polygon", "raster")
  if (poly) {
    res <- list(
      resolution = sf::st_area(grd[1, ]),
      units = units(sf::st_area(grd[1, ]))
    )
  } else {
    res <- stars::st_dimensions(grd)
  }
  ncells <- ifelse(poly, nrow(grd), dim(grd))
  list(
    type = type_text,
    crs = sf::st_crs(grd)$epsg,
    resolution = res,
    bbox = sf::st_bbox(grd),
    ncells = ncells
  )
}

# ------------------------------------------------------------------------------
#' Create a timestamp
#'
#' @export
timestamp <- function() format(Sys.time(), format = "%Y-%m-%d")

# ------------------------------------------------------------------------------
#' Remove and then add whitespaces again
#'
#' @param string character string to modify
#'
#' @export
trim_then_add <- function(string) {
  string <- stringr::str_trim(string, side = "both")
  string <- glue::glue(" {string} ")
  string
}


# ------------------------------------------------------------------------------
#' Subset data queried using pipedat functions
#'
#' This function is used to subset the data based on a bounding box and/or a time span 
#'
#' @param dat object to filter  
#' @eval dp_params()
#' @param data_crs spatial projection of object to filter, defaults to sf::st_crs(dat)"
#'
#' @export
#' @describeIn filter crop object based on bounding box
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
      xmin = min(xy[, "X"]),
      ymin = min(xy[, "Y"]),
      xmax = max(xy[, "X"]),
      ymax = max(xy[, "Y"])
    )
    uid <- (dat$longitude >= bbox["xmin"] & dat$longitude <= bbox["xmax"]) &
      (dat$latitude >= bbox["ymin"] & dat$latitude <= bbox["ymax"])
    dat[uid, ]
  }
}

#' @export
#' @describeIn filter filter object by year
# Filter by year
# Years must be in column "year"
timespan_filter <- function(dat, timespan) {
  dat |>
    dplyr::filter((!!rlang::sym("year")) %in% timespan)
}

#' @export
#' @describeIn filter crop object based on bounding box and filter based on year
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

# ------------------------------------------------------------------------------
#' Create polygon from bbox
#'
#' @param bbox bounding box to spatially subset the queried data, if applicable. The bounding box should be of the form `c(xmin, ymin, xmax, ymax)`",
#' @param crs spatial projection of bounding box argument, defaults to epsg: 4326",
#'
#' @export
bbox_poly <- function(bbox, crs = 4326) {
  sf::st_bbox(bbox, crs = sf::st_crs(crs)) |>
    sf::st_as_sfc()
}
