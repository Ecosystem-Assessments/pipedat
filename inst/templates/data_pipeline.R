#' Shortname of dataset to be queried (dpid:{{ dpid }})
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
  pipeload(urls, glue("{output}{{{ dpid }}}/data-raw/"))
    
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
  metadata(
      id = "{{ dpid }}",
      name = "Provide short name for queried dataset",
      description = "Longer description of the dataset",
      access_date = format(Sys.time(), format="%Y-%m-%d"), 
      period = c(begin, end),
      contact_list = "Need to think about how to do this", # TODO: Find logical way to deal with this
      pipeline_id = "{{ uuid }}",
      data_url = "URL to access the data or its description, if applicable",
      data_uuid = "Unique identifier of the dataset, if available e.g. from API or open data portals",
      package = "Name of package used to access the data, if applicable",
      availability = "Availability of dataset, one of c('open','on demand','data sharing agreement','restricted')",
      citekey = c("citekey1","citekey2","...") # Citation key for reference to bibtex files
  ) 
  
  # TODO: How to add more info
  
  
  
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # CREATE BIBTEX
  # NOTE: mandatory
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # EXPORT 
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # Formatted data 
  fm <- glue("{output}{{{ dpid }}}/data-format/")
  
  # Metadata
  mt <- glue("{output}{{{ dpid }}}/{{ dpid }}.yaml")

  # Bibtex
  bi <- glue("{output}{{{ dpid }}}/{{ dpid }}.bib")
}
