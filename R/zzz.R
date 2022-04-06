#' pipedat
#'
#' @name pipedat
#' @docType package
#' @description Pipelines to access, load, and format data from various sources
#' @importFrom glue glue glue_sql
#' @importFrom whisker whisker.render

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
# Create output folders for data pipelines
makeOutput <- function(uid, output = NULL) {
  # Output
  output <- ifelse(is.null(output), "data/", output)

  # Check if output ends with a "/" to create proper path
  nc <- nchar(output)
  last_char <- ifelse(substr(output, nc, nc) == "/", TRUE, FALSE)
  out <- ifelse(last_char, output, glue("{output}/"))

  # Names of output folders
  l <- list(
    glue("{out}data-raw/{uid}/"),
    glue("{out}data-format/{uid}/"),
    glue("{out}data-metadata/"),
    glue("{out}data-bib/")
  )

  # Create folders if they do not exist
  # for(i in 1:length(l)) if (!file.exists(l[[i]])) dir.create(l[[i]], recursive = TRUE)
  invisible(
    lapply(l, function(x) if (!file.exists(x)) dir.create(x, recursive = TRUE))
  )
}
