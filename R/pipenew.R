#' Creates a new files from templates
#'
#' This function is used to create new files from templates available in the `pipedat` package.
#' This function is meant to facilitate writing of data pipeline for the `pipedat`
#' developers and contributors.
#' Templates available are data pipelines, data connects, and data workflows.
#'
#' @param name for a data pipeline or a data connect, name of the data that will be accessed through this new data pipeline. The name should be short and will only be used to ease the readability of the file structure, as each data pipeline is identified by a unique identifier rather than its name. For a data workflow, name of the data workflow; defaults to `data_workflow`
#' @param template name of the template to generate, one of `data` , `data_govcan` or `workflow`. Defaults to `data_workflow`, as it is the most likely template to be needed by a user of the package.
#'
#' @return This function creates new files from the template available in `inst/templates/`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # New data workflow
#' pipenew()
#'
#' # New data pipeline
#' pipenew("dfo_logbooks", template = "data")
#'
#' # New data connect
#' pipenew("fisheries_intensity", template = "data")
#' }
pipenew <- function(name = NULL, template = "workflow") {
  pip <- template == "data"
  pdg <- template == "data_govcan"
  if (pip | pdg) {
    # Data for template
    out <- list()
    out$dpid <- rnd_id() # Create unique ID of length 8
    out$date_created <- timestamp()
    out$name <- name

    # Update data/data_pipelines.rda
    if (pip) append_dp(pipeline_id = out$dpid, name = out$name)
    if (pdg) {
      append_dp(
        pipeline_id = out$dpid,
        name = out$name,
        url = "https://open.canada.ca/data/en/dataset/",
        avail = "open"
      )
    }

    # Generate template
    if (pip) {
      use_template(
        template = "templates/pipeline.R",
        data = out,
        save_as = glue::glue("inst/pipelines/{name}-{out$dpid}.R")
      )
    }
    if (pdg) {
      use_template(
        template = "templates/pipeline_govcan.R",
        data = out,
        save_as = glue::glue("inst/pipelines/{name}-{out$dpid}.R")
      )
    }
  }

  if (template == "workflow") {
    out <- here::here("project-data", "pipedat")
    chk_create(out)
    if (is.null(name)) name <- "pipeflow"
    use_template(
      template = "templates/pipeflow.yml",
      save_as = glue::glue("project-data/pipedat/{name}.yml")
    )
  }
}

# ------
# Generate random id of length 8 and make sure that it is not duplicated
rnd_id <- function() {
  exist_id <- pipeline$pipeline_id

  # Generate new id that is different from existing ones
  # NOTE: This is likely an overkill, chances are very slim, but who knows!
  i <- TRUE
  while (i) {
    uid <- ids::random_id(1, 4)
    i <- ifelse(uid %in% exist_id, TRUE, FALSE)
  }

  return(uid)
}
