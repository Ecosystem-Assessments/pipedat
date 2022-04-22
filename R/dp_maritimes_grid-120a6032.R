# ------------------------------------------------------------------------------
# Using custom function to write certain metadata information only once, 
# as they appear in the function metadata and the data/pipeline metadata as well
shortname_120a6032 <- function() {
  "Maritimes cumulative effects assessment study area grid"
}
desc_120a6032 <- function() {
  "Gridded study area used jointly with N. Kelly and G. Murphy for the Maritimes region cumulative effects assessment"
}
citekey_120a6032 <- function() {
  c("kelly2021")  
}
# ------------------------------------------------------------------------------

#' @eval shortname_120a6032()
#'
#' @eval desc_120a6032()
#'
#' @eval doc_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 120a6032
#'
#' @examples
#' \dontrun{
#' dp_120a6032()
#' }
dp_120a6032 <- function(output, name = NULL, input = NULL, crs = 4326, bbox = NULL, timespan = NULL, ...) {
  # Output folders
  name <- ifelse(is.null(name), "maritimes_grid", name)
  uid <- "120a6032"
  output <- make_output(uid, name, output)
  path <- glue("{output}{name}-{uid}/")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  msg_local(dir.exists(output))
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
  # TODO: Make a function out of this
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
    pipeline_id = uid,
    # List of creators of the form 
    # `list(people(first_name, last_name, email, organization, department, role))`
    pipeline_creators = people(developer = "david"),
    pipeline_date = "2022-04-22",
    pipeline_url = glue("https://github.com/Ecosystem-Assessments/pipedat/blob/main/R/dp_maritimes_grid-120a6032.R"),
    data_pipeline_uuid = ifelse(q, "83650307-e2dc-4a00-a02f-de7a176093f5", uuid::UUIDgenerate()),
    data_pipeline_crs = crs,
    data_pipeline_bbox = bbox,
    data_pipeline_timespan = timespan,
    data_name = shortname_120a6032(), # NOTE: function as document header
    data_description = desc_120a6032(), # NOTE: function as document header
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
    data_citekey = citekey_120a6032() # NOTE: function as document header
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
      key = citekey_120a6032()[1], # NOTE: function as document header
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
      key = citekey_120a6032()[2], # NOTE: function as document header
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
  # Data name
  nm <- glue("{name}-{uid}")

  # Formatted data 
  fm <- glue("{path}/{nm}.ext")
  
  # Metadata
  mt <- glue("{path}/{nm}.yaml")
  yaml::write_yaml(meta, mt, column.major = FALSE)

  # Bibtex
  bi <- glue("{path}/{nm}.bib")
  RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
  # _________________________________________________________________________________________ #
}
