#' Creates a new data pipeline function
#'
#' This function is used to create a new data pipeline function from a template.
#' This function is meant to facilitate writing of data pipeline for the `pipedat`
#' developers and contributors
#'
#' @param name name of the data that will be accessed through this new data pipeline. The name should be short and will only be used to ease the readability of the file structure, as each data pipeline is identified by a unique identifier rather than its name.
#'
#' @return This function creates a new file `R/dp_{uid}-{name}.R` from the template `inst/templates/data_pipeline.R`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pipenew("fisheries")
#' }
pipenew <- function(name) {
  # TODO: Add new pipeline to list of pipelines
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
    save_as = glue("R/dp_{out$dpid}-{name}.R")
  )
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
