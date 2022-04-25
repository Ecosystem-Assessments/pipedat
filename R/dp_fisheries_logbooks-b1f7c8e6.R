# ------------------------------------------------------------------------------
# Using custom function to write certain metadata information only once, 
# as they appear in the function metadata and the data/pipeline metadata as well
shortname_b1f7c8e6 <- function() {
  "Commercial fisheries logbooks"
}
desc_b1f7c8e6 <- function() {
  "A compilation of landing data from Zonal Interchange File Format (ZIFF) data between 2000 and 2020"
}
citekey_b1f7c8e6 <- function() {
  c("dfo2021")  
}
# ------------------------------------------------------------------------------

#' @eval shortname_b1f7c8e6()
#'
#' @eval desc_b1f7c8e6()
#'
#' @eval doc_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: b1f7c8e6
#'
#' @examples
#' \dontrun{
#' dp_b1f7c8e6()
#' }
dp_b1f7c8e6 <- function(output, crs = 4326, bbox = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "b1f7c8e6"
  name <- data_pipelines$name[data_pipelines$pipeline_id == uid]
  nm <- glue("{name}-{uid}")
  output <- make_output(uid, name, output, local = TRUE)
  path <- glue("{output}{nm}/")
    
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # IMPORT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Function to import ZIFF data
  import_ziff <- function(filename) {
    read.csv(glue("{path}raw/{filename}")) |>
    dplyr::filter(!is.na(latit_GIS) & !is.na(longit_GIS)) |> # Remove NAs
    sf::st_as_sf(coords = c("longit_GIS","latit_GIS"),
                 crs = 4326)
  }

  d <- list()
  d[[1]] <- import_ziff("Version_totale_20002004.csv")
  d[[2]] <- import_ziff("Version_totale_20052009.csv")
  d[[3]] <- import_ziff("Version_totale_20102014.csv")
  d[[4]] <- import_ziff("Version_totale_20152019.csv")
  d[[5]] <- import_ziff("Version_totale_20202024.csv")
  dat <- bind_rows(d)
  # _________________________________________________________________________________________ #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # FORMAT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  dat$year <- format(as.Date(dat$date_cap), format = "%Y")
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
    data_pipeline_uuid = "0d448a4b-1659-433a-937e-cc55a2fc1e27",
    data_pipeline_crs = crs,
    data_pipeline_bbox = bbox,
    data_pipeline_timespan = timespan,
    data_name = shortname_b1f7c8e6(), # NOTE: function as document header
    data_description = desc_b1f7c8e6(), # NOTE: function as document header
    data_access = "2021-06-11",
    data_temporal = unique(dat$year), # c(2000,2001,2002,2003),
    data_bbox = sf::st_bbox(dat),
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
    data_citekey = citekey_b1f7c8e6() # NOTE: function as document header
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
      key = citekey_b1f7c8e6()[1], # NOTE: function as document header
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
      key = citekey_b1f7c8e6()[2], # NOTE: function as document header
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
  fm <- glue("{path}/{nm}.geojson") # NOTE: not necessarily spatial data
  sf::st_write(dat, dsn = fm, quiet = TRUE) # for spatial data
  
  # Metadata
  mt <- glue("{path}/{nm}.yaml")
  yaml::write_yaml(meta, mt, column.major = FALSE)

  # Bibtex
  bi <- glue("{path}/{nm}.bib")
  RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
  # _________________________________________________________________________________________ #
}
