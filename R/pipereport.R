#' Create a report from the pipedat data in a project
#'
#' This function creates a generic report that provides a summary of the data that was loaded and formatted throught the pipedat package in a specific location. The purpose of this report is to accompany a project report by providing the relevant information concerning the data that was accessed and formated through pipedat.
#'
#' @return This function creates a new report in `pubs/pipedat/` using the template `inst/templates/report.Rmd` and all the data available in `project-data/pipedat/` and `project-data/grid/`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate report
#' pipereport()
#' }
pipereport <- function() {
  out <- list()
  out$out <- here::here("pubs", "pipedat")
  chk_create(out$out)

  if (!file.exists(here::here(out$out, "index.Rmd"))) {
    rmarkdown::draft(
      file = here::here(out$out, "index.Rmd"),
      template = "report",
      package = "pipedat"
    )
  }

  # Copy bibtex file
  file.copy(
    from = here::here("project-data", "pipedat", "pipedat.bib"),
    to = here::here("pubs", "pipedat", "pipedat.bib"),
    overwrite = TRUE
  )

  # Render report
  suppressWarnings({
    setwd("./pubs/pipedat/")
    bookdown::render_book(
      input = "index.Rmd",
      output_format = "bookdown::gitbook",
      config_file = "_bookdown.yml"
    )
    setwd("../../")
    # file.copy("./figures/", "./report/", recursive = TRUE)
  })
  utils::browseURL("pubs/pipedat/docs/index.html")
}
