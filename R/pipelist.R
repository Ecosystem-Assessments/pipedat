#' List of available data pipalines
#'
#' This function provides the list of individual scripts
#' built to access data programmatically and reproducibly, which
#' we refer to as *data pipelines*.
#'
#' @param type type of pipeline, one of `data` or `connect`
#'
#' @return This function prints a list of available data pipelines in the `pipedat` package
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pipelist()
#' }
pipelist <- function(type = "data") {
  # Data
  data_description <- data_name <- pipeline_id <- NULL
  uid <- pipeline$pipeline_type == type
  dat <- pipeline[uid, ] |>
    dplyr::select(pipeline_id, data_name, data_description)
  key <- lapply(dat$pipeline_id, get_citekey)
  dat$cite <- unlist(
    lapply(
      key,
      function(x) {
        RefManageR::Citet(
          bib,
          x,
          .opts = list(max.names = 2, longnamesfirst = FALSE)
        )
      }
    )
  )

  # Table
  knitr::kable(
    dat,
    col.names = c("Pipeline ID", "Name", "Description", "Source"),
    row.names = FALSE
  )
}
