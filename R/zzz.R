#' pipedat: pipelines to access, load, and format data from various sources
#'
#' @docType package
#' @name pipedat
#'
#' @importFrom glue glue glue_sql
#' @importFrom RefManageR BibEntry WriteBib
#' @importFrom knitr kable
#' @importFrom readr read_csv
#' @importFrom rlang sym
#' @importFrom R.utils gunzip
#' @importFrom stars read_stars write_stars
#' @importFrom utils read.csv write.csv
#' @importFrom whisker whisker.render
#' @importFrom yaml yaml.load_file write_yaml read_yaml
NULL


# ------------------------------------------------------------------------------
# Gracieuseté de Kevin Cazelles: https://github.com/KevCaz
# my simple(r) version of use template
use_template <- function(template, save_as = stdout(), pkg = "pipedat", ...) {
  template <- readLines(
    fs::path_package(package = pkg, template)
  )
  # NB by default whisker forward the parent envi and I used this
  writeLines(whisker::whisker.render(template, ...), save_as)
}

# ------------------------------------------------------------------------------
# Timestamp
timestamp <- function() format(Sys.time(), format = "%Y-%m-%d")

# ------------------------------------------------------------------------------
# add new data to list of pipelines
append_dp <- function(pipeline_id, name, type) {
  dat <- utils::read.csv("inst/extdata/pipeline.csv")
  dat <- dplyr::bind_rows(
    dat,
    c(
      pipeline_id = pipeline_id,
      pipeline_type = type,
      date_created = timestamp(),
      data_shortname = name
    )
  )
  write.csv(dat, "inst/extdata/pipeline.csv", row.names = FALSE)
}

# ------------------------------------------------------------------------------
# Applying pipeline arguments set by user
bbox_poly <- function(bbox, crs) {
  sf::st_bbox(bbox, crs = sf::st_crs(crs)) |>
    sf::st_as_sfc()
}

# Intersection with bounding box
bbox_crop <- function(dat, bbox, crs) {
  bb <- bbox_poly(bbox, crs)
  if ("sf" %in% class(dat)) {
    sf::st_intersection(dat, bb)
  } else if ("stars" %in% class(dat)) {
    sf::st_crop(dat, bb)
  } else if (class(dat) == "data.frame") {
    uid <- (dat$longitude >= bbox$xmin & dat$longitude <= bbox$xmax) &
      (dat$latitude >= bbox$ymin & dat$latitude <= bbox$ymax)
    dat[uid, ]
  }
}

# Filter by year
# Years must be in column "year"
timespan_filter <- function(dat, timespan) {
  dat |>
    dplyr::filter((!!rlang::sym("year")) %in% timespan)
}

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

# ------------------------------------------------------------------------------
# Create output folders for data pipelines
make_output <- function(uid, name, output = "data", type) {
  if (type == "data") {
    path <- here::here(output, "data-raw", glue("{name}-{uid}"))
    dir.create(here::here(path,"raw"), recursive = TRUE)      
  }
  
  if (type == "integrated") {
    path <- here::here(output, "data-integrated", glue("{name}-{uid}"))
    dir.create(path, recursive = TRUE)      
  }
  
  # Return output path
  invisible(path)
}

# ------------------------------------------------------------------------------
# Helper messages / check functions
# Data needed locally
check_data <- function(filepath, path) {
  if (!file.exists(filepath)) {
    stop(glue("This data is unavailable remotely. The raw data needs to be manually inserted in the folder `{path}/raw/` for the pipeline to work."))
  }
}

# ------------------------------------------------------------------------------
# Update R/sysdata.rda
# Move to a separate file in inst/extdata at some point
update_rda <- function() {
  pipeline <- read.csv(file = "inst/extdata/pipeline.csv")
  contact <- read.csv(file = "inst/extdata/contact.csv")
  pcite <- read.csv(file = "inst/extdata/pipeline_citekey.csv")
  pcontact <- read.csv(file = "inst/extdata/pipeline_contact.csv")
  pcreator <- read.csv(file = "inst/extdata/pipeline_creator.csv")
  bib <- RefManageR::ReadBib("inst/extdata/pipedat.bib")

  usethis::use_data(
    pipeline,
    contact,
    pcite,
    pcontact,
    pcreator,
    bib,
    internal = TRUE,
    overwrite = TRUE
  )
}
