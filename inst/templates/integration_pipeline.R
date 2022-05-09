#' @eval get_name("{{ dpid }}")
#'
#' @eval get_description("{{ dpid }}")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: {{ dpid }}
#'
#' @examples
#' \dontrun{
#' di_{{ dpid }}()
#' }
di_{{ dpid }} <- function(output, grid = NULL, ...) {
  # Output folders and other objects used
  uid <- "{{ dpid }}"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # IMPORT DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  data_id <- c("") # String with data to import
  
  # _________________________________________________________________________________________ #
      
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # ANALYZE / FORMAT DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # INCORPORATE TO STUDY GRID
  # NOTE: optional, only if applicable
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (is.null(grid)) {
    grd_pol <- sf::st_read("data/data-grid/grid_poly.geojson", quiet = TRUE)
    grd_ras <- stars::read_stars("data/data-grid/grid_raster.tif", quiet = TRUE)
  }
  # _________________________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # CREATE METADATA
  # WARNING: mandatory
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- get_metadata(
    pipeline_id = uid,
    integration_date = timestamp(), 
    integration_data = data_id
  )
  
  # To add additional metadata for queried data
  meta <- add_metadata(meta, 
    info1 = c("Format as lists and dataframes to be rendered as yaml"),
    info2 = c("Formatting thus matters"),
    info3 = c("Go to https://github.com/vubiostat/r-yaml for more information")
  )  
  # _________________________________________________________________________________________ #
  

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # EXPORT 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Formatted data 
  fm <- here::here(path,glue("{nm}.geojson")) # NOTE: not necessarily spatial data
  sf::st_write(dat, dsn = fm, quiet = TRUE) # for spatial data

  # Metadata
  mt <- here::here(path,glue("{nm}.yaml"))
  yaml::write_yaml(meta, mt, column.major = FALSE)
  # _________________________________________________________________________________________ #
}
