#' Shortname of dataset to be queried (uid:{{ dpid }})
#'
#' Short description of the dataset to be queried through this data pipeline
#'
#' @eval my_params()
#'
#' @family pipeline functions
#' @rdname pipeline
#' @seealso \code{\link{pipedat}}
#'
#' @examples
#' \dontrun{
#' dp_{{ dpid }}()
#' }

dp_{{ dpid }} <- function(output, input = NULL, bbox = NULL, timespan = NULL, ...) {
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # If the data is downloaded from online sources
  urls <- c(
    "url1",
    "url2",
    "..."
  )
  pipeload(urls, glue("{output}data-raw/{{{ dpid }}}/"))
    
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # IMPORT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # FORMAT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # CREATE METADATA
  # NOTE: mandatory
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  metadata <- list()
  metadata$data_pipeline$pdid <- "{{ pdid }}"
  metadata$data_pipeline$uuid <- "{{ uuid }}"
  
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # CREATE BIBTEX
  # NOTE: mandatory
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # EXPORT 
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  fm <- glue("{output}/data-format/{{{ dpid }}}.R")
  mt <- glue("{output}/data-metadata/{{{ dpid }}}.R")
  bi <- glue("{output}/data-bibtex/{{{ dpid }}}.R")
  
}
