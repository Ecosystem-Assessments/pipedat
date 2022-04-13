# ------------------------------------------------------------------------------
# Using custom function to write certain metadata information only once, 
# as they appear in the function metadata and the metadata itself
shortname_{{ dpid }} <- function() {
  "Shortname of dataset to be queried"
}
desc_{{ dpid }} <- function() {
  "Short description of the dataset to be queried through this data pipeline"
}
# ------------------------------------------------------------------------------

#' @eval shortname_{{ dpid }}()
#'
#' @eval desc_{{ dpid }}()
#'
#' @eval doc_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords dpid: {{ dpid }}
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
    dpid = "{{ dpid }}",
    # List of creators of the form 
    # `list(people(first_name, last_name, email, affiliation, role))`
    pipeline_creators = people(developer = "david"),
    pipeline_date = "{{ date_created }}",
    pipeline_url = glue("https://github.com/Ecosystem-Assessments/pipedat/blob/main/R/dp_{{ dpid }}-{{ name }}.R"),
    data_name = shortname_{{ dpid }}(),
    data_description = desc_{{ dpid }}(), 
    data_access = timestamp(),
    data_temporal = c(2000,2001,2002,2003),
    data_bbox = c(xmin=-1,ymin=-1,xmax=1,xmin=1), # could also use sf::st_bbox()
    data_contacts = list(people("first","last","mail","org","role")), # Same way as creators
    data_url = "https://path/to/data/",
    data_uuid = "f612e2b4-5c67-46dc-9a84-1154c649ab4e",
    data_availability = "open", # 'open','on demand','data sharing agreement','restricted'
    data_citekey = c("citekey1","citekey2","...")
  )
  
  # To add additional metadata for queried data
  meta <- add_meta(meta, 
    info1 = c("Format as lists and dataframes to be rendered as yaml"),
    info2 = c("Formatting thus matters"),
    info3 = c("Go to https://github.com/vubiostat/r-yaml for more information")
  )
  

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
  yaml::write_yaml(meta, mt, column.major = FALSE)

  # Bibtex
  bi <- glue("{output}{{{ dpid }}}/{{ dpid }}.bib")
}
