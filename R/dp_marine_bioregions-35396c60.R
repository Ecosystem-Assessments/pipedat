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
  c("dfo2009", "dfo2010", "goc2011", "dfo2021")
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
dp_35396c60 <- function(output, name = NULL, crs = 4326, bbox = NULL, timespan = NULL, ...) {
  # Output folders
  name <- ifelse(is.null(name), "marine_bioregions", name)
  output <- make_output("35396c60", name, output)
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  path <- glue("{output}{name}-35396c60/")
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
  # SUBSET DATA
  # NOTE: optional, only if applicable
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Bounding bbox
  if (!is.null(bbox)) {
    dat <- bbox_crop(dat, bbox, crs)
  }

  if (!is.null(timespan)) {
    dat <- timespan_filter(dat, timespan, "column")
  }
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
    RefManageR::BibEntry(
      bibtype = "techreport",
      key = citekey_35396c60()[1], # NOTE: function as document header
      author = "{Fisheries and Oceans Canada}",
      year = "2009",
      title = "Development of a Framework and Principles for the Biogeographic Classification of Canadian Marine Areas",
      institution = "{Fisheries and Oceans Canada}",
      type = "{DFO Can. Sci. Advis. Sec. Sci. Advis. Rep. 2009/056}",
      number = "{}",
      url = "https://waves-vagues.dfo-mpo.gc.ca/Library/338958.pdf"
    ),
    RefManageR::BibEntry(
      bibtype = "techreport",
      key = citekey_35396c60()[2], # NOTE: function as document header
      author = "{Fisheries and Oceans Canada}",
      year = "2010",
      title = "Proceedings of a National Science Advisory Process to Provide Guidance on the Development of a Framework and Principles for the Biogeographic Classification of Canadian Marine Areas; 15-16 June 2009",
      institution = "{Fisheries and Oceans Canada}",
      type = "{DFO Can. Sci. Advis. Sec. Proceed. Ser. 2009/039.}",
      url = "https://www.dfo-mpo.gc.ca/csas-sccs/publications/pro-cr/2009/2009_039-eng.htm"
    ),
    RefManageR::BibEntry(
      bibtype = "techreport",
      key = citekey_35396c60()[3], # NOTE: function as document header
      author = "{Government of Canada}",
      year = "2011",
      title = "National Framework for Canada’s Network of Marine Protected Areas",
      institution = "{Fisheries and Oceans Canada}",
      type = "{Ottawa. 31 pp}",
      url = "https://waves-vagues.dfo-mpo.gc.ca/Library/345207.pdf"
    ),
    RefManageR::BibEntry(
      bibtype = "techreport",
      key = citekey_35396c60()[4], # NOTE: function as document header
      author = "{Fisheries and Oceans Canada}",
      year = "2021",
      title = "Federal Marine Bioregions",
      institution = "{Fisheries and Oceans Canada}",
      type = "{}",
      urldate = timestamp(),
      number = "{}",
      url = "https://open.canada.ca/data/en/dataset/23eb8b56-dac8-4efc-be7c-b8fa11ba62e9"
    )
  )
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # EXPORT
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Formatted data (can be multiple datasets)
  nm <- "federal_marine_bioregions-35396c60.geojson"
  fm <- glue("{path}clean/{nm}")
  sf::st_write(dat, dsn = fm, quiet = TRUE)

  # Metadata
  mt <- glue("{path}35396c60.yaml")
  yaml::write_yaml(meta, mt, column.major = FALSE)

  # Bibtex
  bi <- glue("{path}35396c60.bib")
  RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
  # _________________________________________________________________________________________ #
}
