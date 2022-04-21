# ------------------------------------------------------------------------------
# Using custom function to write certain metadata information only once,
# as they appear in the function metadata and the data/pipeline metadata as well
shortname_35396c60 <- function() {
  "Federal Marine Bioregions"
}
desc_35396c60 <- function() {
  "The spatial planning framework for Canada's national network of Marine Protected Areas (MPAs) is comprised of 13 ecologically defined bioregions that cover Canada's oceans and the Great Lakes."
}
citekey_35396c60 <- function() {
  c("dfo2009", "dfo2010", "goc2011", "dfo2022")
}
# ------------------------------------------------------------------------------

#' @eval shortname_35396c60()
#'
#' @eval desc_35396c60()
#'
#' @eval doc_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 35396c60
#'
#' @examples
#' \dontrun{
#' dp_35396c60()
#' }
dp_35396c60 <- function(output, input = NULL, crs = 4326, bbox = NULL, timespan = NULL, ...) {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  path <- glue("{output}35396c60/")
  govcan <- "23eb8b56-dac8-4efc-be7c-b8fa11ba62e9"
  pipeload(govcan = govcan, output = glue("{path}raw"))
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # IMPORT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  dat <- sf::st_read(
    glue("{path}raw/DFO_Marine_Bioregions/DFO_Marine_Bioregions.gdb"),
    layer = "DFO_Marine_Bioregions",
    quiet = TRUE
  )
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # FORMAT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  dat <- sf::st_transform(dat, crs = crs)
  # _________________________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # CREATE METADATA
  # WARNING: mandatory
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  q <- is.null(bbox) & is.null(timespan)
  meta <- metadata(
    pipeline_id = "35396c60",
    # List of creators of the form
    # `list(people(first_name, last_name, email, affiliation, role))`
    pipeline_creators = people(developer = "david"),
    pipeline_date = "2022-04-21",
    pipeline_url = glue("https://github.com/Ecosystem-Assessments/pipedat/blob/main/R/dp_35396c60-marine_bioregions.R"),
    data_pipeline_uuid = ifelse(q, "8d0e2a43-7da9-40f1-8f1d-dfe5b9032e04", uuid::UUIDgenerate()),
    data_pipeline_crs = crs,
    data_pipeline_bbox = bbox,
    data_pipeline_timespan = timespan,
    data_name = shortname_35396c60(), # NOTE: function as document header
    data_description = desc_35396c60(), # NOTE: function as document header
    data_access = timestamp(),
    data_bbox = sf::st_bbox(dat),
    data_contacts = list(
      people(
        email = "DFO.NCRMPCGIS-PCMSIGNCR.MPO@dfo-mpo.gc.ca",
        organization = "Fisheries and Oceans Canada",
        department = "Marine Planning and Conservation Directorate"
      )
    ),
    data_url = "https://open.canada.ca/data/en/dataset/23eb8b56-dac8-4efc-be7c-b8fa11ba62e9",
    data_uuid = "23eb8b56-dac8-4efc-be7c-b8fa11ba62e9",
    data_availability = "open", # 'open','on demand','data sharing agreement','restricted'
    data_citekey = citekey_35396c60() # NOTE: function as document header
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
      key = citekey_35396c60()[1], # NOTE: function as document header
      author = "Last_name, First_name and Last_Name, First_name and {Organisation name}",
      year = "2018",
      title = "Title of the dataset",
      institution = "{}",
      type = "{}",
      urldate = timestamp(),
      number = "{}",
      url = "https://path/to/data",
      doi = "doi of data"
    ),
    # For a journal article
    RefManageR::BibEntry(
      bibtype = "article",
      key = citekey_35396c60()[2], # NOTE: function as document header
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
  # Formatted data (can be multiple datasets)
  nm <- "federal_marine_bioregions-35396c60.geojson"
  fm <- glue("{path}format/{nm}")
  sf::st_write(dat, dsn = fm, quiet = TRUE)

  # Metadata
  mt <- glue("{path}35396c60.yaml")
  yaml::write_yaml(meta, mt, column.major = FALSE)

  # Bibtex
  bi <- glue("{path}35396c60.bib")
  RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
  # _________________________________________________________________________________________ #
}
