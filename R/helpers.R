#' Check if folder exists and create if not
#'
#' @param path path of folder to use as output, create if it does not already exist
#'
#' @export
chk_create <- function(path) {
  if (!file.exists(path)) dir.create(path, recursive = TRUE)
}

# ------------------------------------------------------------------------------
#' Series of functions to process the data and integration pipelines
#'
#' @param uid unique identifier of queried data.
#'
#' @export
#' @describeIn pipeline_setup base path for all data formatted through pipedat
write_pipeline <- function(uid) {
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  use_template(
    template = glue::glue("pipelines/{nm}.R"),
    save_as = glue::glue("data/pipedat/{nm}/{nm}.R")
  )  
}


# ------------------------------------------------------------------------------
#' Series of functions to process the data and integration pipelines
#'
#' @param uid unique identifier of queried data.
#'
#' @export
#' @describeIn pipeline_setup base path for all data formatted through pipedat
make_path <- function(uid) {
  here::here(
    "data",
    "pipedat",
    glue::glue("{pipedat::get_shortname(uid)}-{uid}")
  )
}


#' @export
#' @describeIn pipeline_setup check if raw data exists
check_raw <- function(uid) {
  path <- make_path(uid)
  name <- get_shortname(uid)
  rawpath <- here::here(path, "raw")
  execute <- !file.exists(here::here(path, glue::glue("{name}-{uid}-raw.tar.xz"))) &
             length(dir(rawpath)) == 0
  if (execute) {
    path <- here::here(
      path,
      "raw"
    )
    chk_create(path)    
  }
  invisible(execute)
}

#' @export
#' @describeIn pipeline_setup check if formatted data exists
check_format <- function(uid) {
  path <- make_path(uid)
  name <- get_shortname(uid)
  format <- here::here(path, "format")
  execute <- !file.exists(format) | 
             length(dir(format)) == 0
  if (execute) {
    # Create folder 
    chk_create(format)
    
    # If raw data is compressed only, decompress 
    if (!file.exists(here::here(path,"raw"))) {
      archive::archive(here::here(path,glue::glue("{name}-{uid}-raw.tar.xz")))
    }
  }
  invisible(execute)
}

#' @export
#' @describeIn pipeline_setup check if integrated data exists
check_integrated <- function(uid) {
  path <- make_path(uid)
  integrated <- here::here(path, "integrated")
  execute <- !file.exists(integrated) | 
             length(dir(integrated)) == 0
  if (execute) {
    chk_create(integrated)
  }
  invisible(execute)
}

#' @export
#' @describeIn pipeline_setup create raw.zip if it does not exist and remove raw/
clean_path <- function(uid, keep_raw = TRUE) {
  path <- make_path(uid)
  name <- get_shortname(uid)
  rawpath <- here::here(path, "raw")
  rawzip <- here::here(path, glue::glue("{name}-{uid}-raw.tar.xz"))
             
  # if raw/ exists but compressed file does not and keep_raw is true
  if (!file.exists(rawzip) & file.exists(rawpath) & keep_raw) {
    archive::archive_write_dir(
      archive = rawzip,
      dir = rawpath,
      recursive = TRUE
    )
  }
  
  # If keep_raw is false and compressed file exists
  if (!keep_raw) unlink(rawzip)
  
  # if raw/ exists
  if (file.exists(rawpath)) unlink(rawpath, recursive = TRUE)
}

#' @describeIn pipeline_setup get path to files of a pipeline
#' @export
get_filepaths <- function(uid) {
  path <- make_path(uid)
  rawpath <- here::here(path, "raw")
  fmtpath <- here::here(path, "format")
  intpath <- here::here(path, "integrated")
  filepaths <- list()
  filepaths$raw <- dir(rawpath, recursive = TRUE, full.names = TRUE)
  filepaths$format <- dir(fmtpath, recursive = TRUE, full.names = TRUE)
  filepaths$integrated <- dir(intpath, recursive = TRUE, full.names = TRUE)
  invisible(filepaths)
}



# ------------------------------------------------------------------------------
#' Series of functions to access metadata information
#'
#' @param uid unique identifier of queried data.
#'
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
  repo <- "https://github.com/Ecosystem-Health/pipedat"
  glue::glue("{repo}/blob/main/R/{dat$data_shortname}-{uid}.R")
}

#' @describeIn pipeline_setup get information on a grid
#' @export
get_grid_info <- function(grd = here::here("data/grid/grid.tif")) {
  # Get grid
  if (class(grd) == "character") grd <- stars::read_stars(grd)
  if (!"stars" %in% class(grd)) grd <- stars::st_as_stars(grd)

  # Info 
  list(
    crs = sf::st_crs(grd)$epsg,
    resolution = stars::st_dimensions(grd),
    bbox = sf::st_bbox(grd),
    ncells = dim(grd)
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


