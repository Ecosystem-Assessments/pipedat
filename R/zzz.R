#' pipedat: pipelines to access, load, and format data from various sources
#'
#' @docType package
#' @name pipedat
#'
#' @importFrom glue glue glue_sql
#' @importFrom RefManageR BibEntry WriteBib
#' @importFrom rlang sym
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
# pipeline url 
pipeline_url <- function(dpid, name) {
  repo <- yaml::read_yaml("DESCRIPTION")$URL
  glue("{repo}/blob/main/R/dp_{name}-{dpid}.R")
}

# ------------------------------------------------------------------------------
# Intersection with bounding box
bbox_crop <- function(dat, bbox, crs) {
  bbox_poly <- sf::st_bbox(bbox, crs = sf::st_crs(crs)) |>
               sf::st_as_sfc(dat)
  sf::st_intersection(dat, bbox_poly)
}

# ------------------------------------------------------------------------------
# Filter by year
timespan_filter <- function(dat, timespan, column) {
  dat |>
    dplyr::filter((!!rlang::sym(column)) %in% timespan)
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
make_output <- function(uid, name, output = NULL) {
  output <- ifelse(is.null(output), "data/data-raw/", output) # default output if NULL
  output <- check_output(output) # Check if output ends with "/"
  newdir <- glue("{output}/{name}-{uid}") # Name of new outdir
  msg_exists(dir.exists(newdir)) # Stop if new dir exists

  # Names of output folders to create
  l <- list(
    glue("{newdir}/raw/")
    # glue("{newdir}/clean/")
  )

  # Create folders if they do not exist
  lapply(l, function(x) if (!file.exists(x)) dir.create(x, recursive = TRUE))
  
  # Return output path 
  invisible(output)
  # TODO: For GitHub, create .gitkeep and modify .gitignore
}

# ------------------------------------------------------------------------------
# Helper messages / check functions
# Data folder already exists
msg_exists <- function(x) {
  if (x) {
    stop("This data already exists in the target output folder. Provide a new output folder or set `overwrite` to TRUE")    
  }
}

# Data needed locally
msg_local <- function(x, path) {
  if (x) {
    stop(glue("This data is unavailable remotely. The raw data needs to be manually inserted in the folder `{path}` for the pipeline to work."))
  }
}
