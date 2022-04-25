#' pipedat: pipelines to access, load, and format data from various sources
#'
#' @docType package
#' @name pipedat
#'
#' @importFrom glue glue glue_sql
#' @importFrom RefManageR BibEntry WriteBib
#' @importFrom rlang sym
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
# Intersection with bounding box
bbox_crop <- function(dat, bbox, crs) {
  bbox_poly <- sf::st_bbox(bbox, crs = sf::st_crs(crs)) |>
    sf::st_as_sfc(dat)
  sf::st_intersection(dat, bbox_poly)
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
# Check if output ends with a "/" to create proper path
check_output <- function(output) {
  if (!is.null(output)) {
    nc <- nchar(output)
    last_char <- ifelse(substr(output, nc, nc) == "/", TRUE, FALSE)
    ifelse(last_char, output, glue("{output}/"))
  } else {
    NULL
  }
}

# ------------------------------------------------------------------------------
# Create output folders for data pipelines
make_output <- function(uid, name, output = NULL, local = FALSE) {
  output <- ifelse(is.null(output), "data/data-raw/", output) # default output if NULL
  output <- check_output(output) # Check if output ends with "/"
  newdir <- glue("{output}{name}-{uid}") # Name of new outdir
  if (!local) msg_exists(dir.exists(newdir)) # Stop if new dir exists
  if (local) msg_local(!dir.exists(newdir), newdir) # Stop if new dir does not exist and should be there
  if (!local) {
    # Names of output folders to create
    l <- list(
      glue("{newdir}/raw/")
      # glue("{newdir}/clean/")
    )

    # Create folders if they do not exist
    lapply(l, function(x) if (!file.exists(x)) dir.create(x, recursive = TRUE))
  }
  # Return output path
  invisible(output)
  # TODO: For GitHub, create .gitkeep and modify .gitignore
}

# ------------------------------------------------------------------------------
# Helper messages / check functions
# Data folder already exists
msg_exists <- function(x) {
  if (x) {
    stop("This data already exists in the target output folder.")
  }
}

# Data needed locally
msg_local <- function(x, path) {
  if (x) {
    stop(glue("This data is unavailable remotely. The raw data needs to be manually inserted in the folder `{path}/raw/` for the pipeline to work. Type `dir.create('{path}/raw/', recursive = TRUE)` to create the folder."))
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
