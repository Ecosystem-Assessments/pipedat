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
  c("citekey1","citekey2")  
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
#' @keywords pipeline_id: {{ dpid }}
#'
#' @examples
#' \dontrun{
#' dp_{{ dpid }}()
#' }
dp_{{ dpid }} <- function(output, input = NULL, crs = 4326, bbox = NULL, timespan = NULL, ...) {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  path <- glue("{output}{{ dpid }}/")
  # If the data is downloaded from online sources
  urls <- c(
    "url1",
    "url2",
    "..."
  )
  
  # If the data is downloaded from open government using `rgovcan`
  govcan <- "govcan uuid"
  
  # Load
  pipeload(urls = urls, govcan = govcan, glue("{path}raw/"))
  # _________________________________________________________________________________________ #
    
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # IMPORT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # dat <- import data function
  
  # _________________________________________________________________________________________ #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # FORMAT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # APPLY SUBSET AND CRS SPECIFIED BY USER
  # NOTE: optional, only if applicable
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Projection
  dat <- sf::st_transform(dat, crs = crs)

  # Bounding bbox
  if (!is.null(bbox)) {
    dat <- bbox_crop(dat, bbox, crs)  
  }
  
  if (!is.null(timespan)) {
    # "column" is the name of the column in which the years are stored in the dataset
    dat <- timespan_filter(dat, timespan, "column")
  }
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # CREATE METADATA
  # WARNING: mandatory
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  q <- is.null(bbox) & is.null(timespan) 
  meta <- metadata(
    pipeline_id = "{{ dpid }}",
    # List of creators of the form 
    # `list(people(first_name, last_name, email, organization, department, role))`
    pipeline_creators = people(developer = "david"),
    pipeline_date = "{{ date_created }}",
    pipeline_url = glue("https://github.com/Ecosystem-Assessments/pipedat/blob/main/R/dp_{{ name }}-{{ dpid }}.R"),
    data_pipeline_uuid = ifelse(q, "{{ uuid }}", uuid::UUIDgenerate()),
    data_pipeline_crs = crs,
    data_pipeline_bbox = bbox,
    data_pipeline_timespan = timespan,
    data_name = shortname_{{ dpid }}(), # NOTE: function as document header
    data_description = desc_{{ dpid }}(), # NOTE: function as document header
    data_access = timestamp(),
    data_temporal = c(), # c(2000,2001,2002,2003),
    data_bbox = c(), # c(xmin=-1,ymin=-1,xmax=1,xmin=1), # could also use sf::st_bbox()
    data_contacts = list(
      people(
        first_name = "first_name",
        last_name = "last_name",
        email = "email",
        organization = "organization",
        department = "department",
        role = "role"
      )
    ), # Same way as creators
    data_url = "https://path/to/data/",
    data_uuid = "unique identifier of raw data or resource",
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
  nm <- "name_of_data-{{ dpid }}.ext"
  fm <- glue("{path}/format/{nm}")
  
  # Metadata
  mt <- glue("{path}/{{ dpid }}.yaml")
  yaml::write_yaml(meta, mt, column.major = FALSE)

  # Bibtex
  bi <- glue("{path}/{{ dpid }}.bib")
  RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
  # _________________________________________________________________________________________ #
}
