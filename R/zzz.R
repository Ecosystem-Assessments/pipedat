#' pipedat: pipelines to access, load, and format data from various sources
#'
#' @docType package
#' @name pipedat
#'
#' @importFrom glue glue glue_sql
#' @importFrom grDevices dev.off png colorRampPalette
#' @importFrom graphics par box layout lines mtext
#' @importFrom graphics polygon text
#' @importFrom RefManageR BibEntry WriteBib
#' @importFrom rlang sym
#' @importFrom sf st_bbox st_write st_read st_transform
#' @importFrom sf st_as_sf st_buffer st_make_valid
#' @importFrom sf st_intersection st_intersects st_crop
#' @importFrom sf st_area st_crs st_make_grid st_as_sfc
#' @importFrom sf st_set_crs sf_use_s2 st_coordinates
#' @importFrom stars read_stars write_stars st_warp
#' @importFrom stars st_dimensions st_as_stars
#' @importFrom stats setNames na.omit
#' @importFrom terra writeRaster
#' @importFrom utils read.csv write.csv read.table
#' @importFrom yaml yaml.load_file write_yaml read_yaml
#' @importFrom rlang abort warn
#' @importFrom cli symbol
#' @importFrom crayon blue
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
#' Check if folder exists and create if not
chk_create <- function(path) {
  if (!file.exists(path)) dir.create(path, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# add new data to list of pipelines
append_dp <- function(pipeline_id, name, url = NULL, avail = NULL) {
  dat <- utils::read.csv("inst/extdata/pipeline.csv")
  dat <- dplyr::bind_rows(
    dat,
    c(
      pipeline_id = pipeline_id,
      date_created = timestamp(),
      data_shortname = name,
      data_url = url,
      data_availability = avail
    )
  )
  write.csv(dat, "inst/extdata/pipeline.csv", row.names = FALSE)
}

# ------------------------------------------------------------------------------
# Get basemap data, to add to R/sysdata.rda
get_basemap <- function() {
  # Quebec + Maritimes provinces
  canada <- raster::getData("GADM", country = "CAN", level = 1, path = "project-data")
  canada <- sf::st_as_sf(canada)
  qc <- canada[canada$NAME_1 == "Qu\u00e9bec", ]
  ns <- canada[canada$NAME_1 == "Nova Scotia", ]
  nb <- canada[canada$NAME_1 == "New Brunswick", ]
  nfl <- canada[canada$NAME_1 == "Newfoundland and Labrador", ]
  pei <- canada[canada$NAME_1 == "Prince Edward Island", ]

  # Canada
  canada <- raster::getData("GADM", country = "CAN", level = 0, path = "project-data")
  canada <- sf::st_as_sf(canada)

  # USA
  usa <- raster::getData("GADM", country = "USA", level = 0, path = "project-data")
  usa <- sf::st_as_sf(usa)

  # Delete loaded data
  files <- dir("./data", pattern = ".rds", full.names = TRUE)
  file.remove(files)

  # Store in list
  basemap <- list()
  suppressWarnings({
    basemap$qc <- sf::st_simplify(qc, dTolerance = 200, preserveTopology = FALSE)
    basemap$ns <- sf::st_simplify(ns, dTolerance = 200, preserveTopology = FALSE)
    basemap$nb <- sf::st_simplify(nb, dTolerance = 200, preserveTopology = FALSE)
    basemap$nfl <- sf::st_simplify(nfl, dTolerance = 200, preserveTopology = FALSE)
    basemap$pei <- sf::st_simplify(pei, dTolerance = 200, preserveTopology = FALSE)
    basemap$can <- sf::st_simplify(canada, dTolerance = 600, preserveTopology = FALSE)
    basemap$usa <- sf::st_simplify(usa, dTolerance = 600, preserveTopology = FALSE)
  })

  # Export
  save(basemap, file = "./inst/extdata/basemap.rds")
}

# ------------------------------------------------------------------------------
# Pipelines as list of functions
# For developers only
get_pipeline_code <- function() {
  # List of available pipelines
  files <- dir("inst/pipelines", recursive = TRUE, full.names = TRUE)
  uid <- substr(files, nchar(files) - 9, nchar(files) - 2)
  pipecode <- lapply(files, source, local = TRUE)
  for (i in 1:length(pipecode)) {
    pipecode[[i]] <- pipecode[[i]]$value
  }
  names(pipecode) <- uid

  # Export
  save(pipecode, file = "./inst/extdata/pipeline_code.rda")
}

# ------------------------------------------------------------------------------
# Update R/sysdata.rda
# Move to a separate file in inst/extdata at some point
update_rda <- function() {
  get_pipeline_code()
  pipeline <- read.csv(file = "inst/extdata/pipeline.csv")
  contact <- read.csv(file = "inst/extdata/contact.csv")
  pcite <- read.csv(file = "inst/extdata/pipeline_citekey.csv")
  pcontact <- read.csv(file = "inst/extdata/pipeline_contact.csv")
  pcreator <- read.csv(file = "inst/extdata/pipeline_creator.csv")
  bib <- RefManageR::ReadBib("inst/extdata/pipedat.bib")
  load(file = "inst/extdata/basemap.rda")
  load(file = "inst/extdata/pipeline_code.rda")


  usethis::use_data(
    pipeline,
    contact,
    pcite,
    pcontact,
    pcreator,
    bib,
    basemap,
    pipecode,
    internal = TRUE,
    overwrite = TRUE
  )
}

# --------------------------------------------------------------------------------
# Helper messages
# Skip download
msgNoLoad <- function(uid) {
  rlang::warn(c(
    glue::glue("The data for the {get_shortname(uid)} pipeline (id: {uid}) is already available on disk, download was thus skipped."),
    "i" = "Delete or move files from disk for data to be downloaded again."
  ))
}

msgNoClean <- function(uid) {
  rlang::warn(c(
    glue::glue("The cleaned data from the {get_shortname(uid)} pipeline (id: {uid}) is already available on disk, data formatting was thus skipped."),
    "i" = "Delete or move files from disk for data to be downloaded again."
  ))
}

# If data is needed locally, stop process
msgOnDisk <- function(uid, paths) {
  rlang::abort(c(
    glue::glue("The data for the {get_shortname(uid)} pipeline (id: {uid}) is unavailable remotely and needs to be on disk for the pipeline to work."),
    "i" = "The raw data should be available at the following paths:",
    "",
    paths$raw_files
  ))
}

msgNoIntegration <- function(uid) {
  rlang::warn(c(
    glue::glue("The integrated data from the {get_shortname(uid)} pipeline (id: {uid}) is already available on disk, data integration was thus skipped."),
    "i" = "Delete or move files from disk for data to be downloaded again."
  ))
}



msgInfo <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$info, ...)
  message(crayon::green(txt), appendLF = appendLF)
  invisible(txt)
}
