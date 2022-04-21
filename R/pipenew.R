#' Creates a new files from templates
#'
#' This function is used to create new files from templates available in the `pipedat` package.
#' This function is meant to facilitate writing of data pipeline for the `pipedat`
#' developers and contributors.
#' Templates available are data pipelines, data connects, and data workflows.
#'
#' @param name for a data pipeline or a data connect, name of the data that will be accessed through this new data pipeline. The name should be short and will only be used to ease the readability of the file structure, as each data pipeline is identified by a unique identifier rather than its name. For a data workflow, name of the data workflow; defaults to `data_workflow`
#' @param template name of the template to generate, one of `data_pipeline`, `data_connect`, `data_workflow`. Defaults to `data_workflow`, as it is the most likely template to be needed by a user of the package.
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
#' pipenew("dfo_logbooks", template = "data_pipeline")
#'
#' # New data connect
#' pipenew("fisheries_intensity", template = "data_connect")
#' }
pipenew <- function(name = NULL, template = "data_workflow") {
  if (template == "data_pipeline") {
    # Data for template
    out <- list()
    out$dpid <- rnd_id() # Create unique ID of length 8
    out$uuid <- uuid::UUIDgenerate() # Create Version 4 UUID for the pipeline
    out$date_created <- timestamp()
    out$name <- name

    # Create "R/" if it does not exist
    if (!file.exists("R/")) dir.create("R/")

    # Generate template
    use_template(
      template = "templates/data_pipeline.R",
      data = out,
      save_as = glue("R/dp_{name}-{out$dpid}.R")
    )
  }

  if (template == "data_connect") {
    # TODO: create template
  }

  if (template == "data_workflow") {
    if (is.null(name)) name <- "data_workflow"
    use_template(
      template = "templates/data_workflow.yaml",
      save_as = glue("./{name}.yaml")
    )
  }
}

# ------
# Generate random id of length 8 and make sure that it is not duplicated
rnd_id <- function() {
  files <- dir("R/")
  dp <- files[stringr::str_detect(files, "dp_")]
  exist_id <- gsub(".*dp_(.+)-.*", "\\1", dp)

  # Generate new id that is different from existing ones
  # NOTE: This is likely an overkill, chances are very slim, but who knows!
  i <- TRUE
  while (i) {
    uid <- ids::random_id(1, 4)
    i <- ifelse(uid %in% exist_id, TRUE, FALSE)
  }

  return(uid)
}
