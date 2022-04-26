#' @eval get_name("{{ dpid }}")
#'
#' @eval get_description("{{ dpid }}")
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
dp_{{ dpid }} <- function(output, crs = 4326, bbox = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "{{ dpid }}"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  output <- make_output(uid, name, output, local = FALSE) # set local = TRUE for local data 
  path <- glue("{output}{nm}/")

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
  
  # If the data is downloaded from open government using `rgovcan`
  govcan <- "govcan uuid"
  
  # Load
  pipeload(urls = urls, govcan = govcan, output = glue("{path}raw/"), large = FALSE)
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
  # CREATE METADATA
  # WARNING: mandatory
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- get_metadata(
    pipeline_id = uid,
    pipeline_crs = crs, 
    pipeline_bbox = bbox, 
    pipeline_timespan = timespan, 
    data_access = timestamp(), 
    data_bbox = sf::st_bbox(dat), 
    data_timespan = sort(unique(dat$year))
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
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  bib <- get_bib(uid)
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
