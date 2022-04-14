# ------------------------------------------------------------------------------
# Using custom function to write certain metadata information only once, 
# as they appear in the function metadata and the data/pipeline metadata as well
shortname_{{ dpid }} <- function() {
  "Shortname of dataset to be queried"
}
desc_{{ dpid }} <- function() {
  "Short description of the dataset to be queried through this data pipeline"
}
citekey_{{ dpid }} <- function() {
  c("citekey1","citekey2","...")  
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
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # If the data is downloaded from online sources
  urls <- c(
    "url1",
    "url2",
    "..."
  )
  pipeload(urls, glue("{output}{{{ dpid }}}/data-raw/"))
  # _________________________________________________________________________________________ #
    
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # IMPORT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  
  
  # _________________________________________________________________________________________ #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # FORMAT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  
  
  # _________________________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # CREATE METADATA
  # WARNING: mandatory
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  q <- is.null(bbox) & is.null(timespan) 
  meta <- metadata(
    pipeline_id = "{{ dpid }}",
    # List of creators of the form 
    # `list(people(first_name, last_name, email, affiliation, role))`
    pipeline_creators = people(developer = "david"),
    pipeline_date = "{{ date_created }}",
    pipeline_url = glue("https://github.com/Ecosystem-Assessments/pipedat/blob/main/R/dp_{{ dpid }}-{{ name }}.R"),
    data_pipeline_uuid = ifelse(q, "{{ uuid }}", uuid::UUIDgenerate()),
    data_pipeline_bbox = bbox,
    data_pipeline_timespan = timespan,
    data_name = shortname_{{ dpid }}(), # NOTE: function as document header
    data_description = desc_{{ dpid }}(), # NOTE: function as document header
    data_access = timestamp(),
    data_temporal = c(2000,2001,2002,2003),
    data_bbox = c(xmin=-1,ymin=-1,xmax=1,xmin=1), # could also use sf::st_bbox()
    data_contacts = list(people("first","last","mail","org","role")), # Same way as creators
    data_url = "https://path/to/data/",
    data_uuid = "f612e2b4-5c67-46dc-9a84-1154c649ab4e",
    data_availability = "open", # 'open','on demand','data sharing agreement','restricted'
    data_citekey = citekey_{{ dpid }}() # NOTE: function as document header
  )
  
  # To add additional metadata for queried data
  meta <- add_metadata(meta, 
    info1 = c("Format as lists and dataframes to be rendered as yaml"),
    info2 = c("Formatting thus matters"),
    info3 = c("Go to https://github.com/vubiostat/r-yaml for more information")
  )  
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # CREATE BIBTEX
  # WARNING: mandatory
  # 
  # NOTE:
  #   Create bibtex entries using `RefManageR::BibEntry()`
  #   For more information on the functions andd available entries visit: 
  #   https://docs.ropensci.org/RefManageR/reference/BibEntry.html
  #   For entry types: https://www.bibtex.com/e/entry-types/
  #   Some guidance on how to cite datasets: 
  #   https://social-science-data-editors.github.io/guidance/citations/guidance_data_citations.pdf
  #   Using the @techreport entry type for datasets, as there are no specific entries for data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  bib <- c(
    # For a dataset
    RefManageR::BibEntry(
      bibtype = "techreport", 
      key = citekey_{{ dpid }}()[1], # NOTE: function as document header
      author = "Last_name, First_name and Last_Name, First_name and {Organisation name}", 
      year = "2018",
      title = "Title of the dataset",
      institution = "{}", 
      type = "{}", 
      urldate =  timestamp(),
      number = "{}", 
      url = "https://path/to/data",
      doi = "doi of data"
    ),
    # For a journal article
    RefManageR::BibEntry(
      bibtype = "article", 
      key = citekey_{{ dpid }}()[2], # NOTE: function as document header
      author = "Last_name, First_name and Last_Name, First_name and {Organisation name}", 
      year = "2018",
      title = "Title of the article",
      journal = "Journal name",
      volume = "1",
      number = "1",
      pages = "1--2",
      publisher = "{Publisher name}",
      issn = "article issn",
      doi = "article doi",
      url = "https://path/to/data"
    )
  ) 
  # _________________________________________________________________________________________ #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # EXPORT 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Formatted data 
  fm <- glue("{output}{{{ dpid }}}/data-format/")
  
  # Metadata
  mt <- glue("{output}{{{ dpid }}}/{{ dpid }}.yaml")
  yaml::write_yaml(meta, mt, column.major = FALSE)

  # Bibtex
  bi <- glue("{output}{{{ dpid }}}/{{ dpid }}.bib")
  RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
  # _________________________________________________________________________________________ #
}
