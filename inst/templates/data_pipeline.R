#' Shortname of dataset to be queried (uid:{{ uid }})
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
#' dp_{{ uid }}()
#' }

dp_{{ uid }} <- function(output, input = NULL, bbox = NULL, timespan = NULL, ...) {
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  urls <- c(
    "url1",
    "url2",
    "..."
  )
  
  # --
  pipeload(urls, glue("{output}data-raw/{{{ uid }}}/"))
    
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
  
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # CREATE BIBTEX
  # NOTE: mandatory
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # EXPORT 
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  fm <- glue("{output}/data-format/{{{ uid }}}.R")
  mt <- glue("{output}/data-metadata/{{{ uid }}}.R")
  bi <- glue("{output}/data-bibtex/{{{ uid }}}.R")
  
}
