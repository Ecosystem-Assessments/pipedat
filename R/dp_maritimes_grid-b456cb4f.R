# ------------------------------------------------------------------------------
# Using custom function to write certain metadata information only once,
# as they appear in the function metadata and the data/pipeline metadata as well
shortname_b456cb4f <- function() {
  "Maritimes cumulative effects assessment study area grid"
}
desc_b456cb4f <- function() {
  "Gridded study area used jointly with N. Kelly and G. Murphy for the Maritimes region cumulative effects assessment"
}
citekey_b456cb4f <- function() {
  c("kelly2021")
}
# ------------------------------------------------------------------------------

#' @eval shortname_b456cb4f()
#'
#' @eval desc_b456cb4f()
#'
#' @eval doc_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: b456cb4f
#'
#' @examples
#' \dontrun{
#' dp_b456cb4f()
#' }
dp_b456cb4f <- function(output, crs = 4326, bbox = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "b456cb4f"
  name <- pipedat::data_pipelines$name[pipedat::data_pipelines$pipeline_id == uid]
  nm <- glue("{name}-{uid}")
  output <- make_output(uid, name, output, local = TRUE)
  path <- glue("{output}{nm}/")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # IMPORT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  dat <- sf::st_read(glue("{path}raw/pu.shp"), quiet = TRUE)
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # FORMAT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  UNIT_ID <- geometry <- NULL
  dat <- sf::st_make_valid(dat)
  dat <- dplyr::select(dat, ID = UNIT_ID, geometry)
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # APPLY SUBSETS AND CRS SPECIFIED BY USER
  # NOTE: optional, only if applicable
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  dat <- dp_parameters(
    dat,
    crs = crs,
    bbox = bbox,
    timespan = timespan
  )
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # CREATE METADATA
  # WARNING: mandatory
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- metadata(
    pipeline_id = uid,
    # List of creators of the form
    # `list(people(first_name, last_name, email, organization, department, role))`
    pipeline_creators = people(developer = "david"),
    pipeline_date = "2022-04-22",
    pipeline_url = pipeline_url(uid, name),
    data_pipeline_uuid = "280a239c-2d18-489a-a6d1-9601c6ff60ba",
    data_pipeline_crs = crs,
    data_pipeline_bbox = bbox,
    data_pipeline_timespan = timespan,
    data_name = shortname_b456cb4f(), # NOTE: function as document header
    data_description = desc_b456cb4f(), # NOTE: function as document header
    data_access = "2021-08-11",
    data_bbox = sf::st_bbox(dat),
    data_contacts = list(
      people(
        first_name = "Noreen",
        last_name = "Kelly",
        email = "Noreen.Kelly@dfo-mpo.gc.ca",
        organization = "Fisheries and Oceans Canada",
        department = "Bedford Institute of Oceanography",
        role = "Research scientist"
      ),
      people(
        first_name = "Grace",
        last_name = "Murphy",
        email = "Grace.Murphy@dfo-mpo.gc.ca",
        organization = "Fisheries and Oceans Canada",
        department = "Bedford Institute of Oceanography",
        role = "Research scientist"
      )
    ), # Same way as creators
    data_availability = "on demand", # 'open','on demand','data sharing agreement','restricted'
    data_citekey = citekey_b456cb4f() # NOTE: function as document header
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
      key = citekey_b456cb4f()[1], # NOTE: function as document header
      author = "Kelly, Noreen and Murphy, Grace",
      year = "2021",
      title = "Maritimes cumulative effects assessment study area grid",
      institution = "{Fisheries and Oceans Canada}",
      type = "{Dataset}",
      urldate = timestamp()
    )
  )
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # EXPORT
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Formatted data
  fm <- glue("{path}/{nm}.geojson")
  sf::st_write(dat, dsn = fm, quiet = TRUE)

  # Metadata
  mt <- glue("{path}/{nm}.yaml")
  yaml::write_yaml(meta, mt, column.major = FALSE)

  # Bibtex
  bi <- glue("{path}/{nm}.bib")
  RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
  # _________________________________________________________________________________________ #
}
