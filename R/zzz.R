#' pipedat: pipelines to access, load, and format data from various sources
#'
#' @docType package
#' @name pipedat
#'
#' @importFrom glue glue glue_sql
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
#' @importFrom utils read.csv write.csv read.table
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

# Create polygon from bbox
bbox_poly <- function(bbox, crs) {
  sf::st_bbox(bbox, crs = sf::st_crs(crs)) |>
    sf::st_as_sfc()
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
  integ <- read.csv(file = "inst/extdata/data_integration.csv")
  files_raw <- read.csv(file = "inst/extdata/files_raw.csv")
  files_clean <- read.csv(file = "inst/extdata/files_clean.csv")
  files_integrated <- read.csv(file = "inst/extdata/files_integrated.csv")

  usethis::use_data(
    pipeline,
    contact,
    pcite,
    pcontact,
    pcreator,
    bib,
    integ,
    files_raw,
    files_clean,
    files_integrated,
    internal = TRUE,
    overwrite = TRUE
  )
}
