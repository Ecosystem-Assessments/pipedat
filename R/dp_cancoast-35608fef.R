#' @eval get_name("35608fef")
#'
#' @eval get_description("35608fef")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 35608fef
#'
#' @examples
#' \dontrun{
#' dp_35608fef()
#' }
dp_35608fef <- function(crs = 4326, bbox = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "35608fef"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  exist <- check_files(uid, name, ondisk = FALSE)
  path <- make_output(uid, name)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!exist$raw) {
    urls <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/publications/STPublications_PublicationsST/314/314669/of_8551.zip"
    govcan <- "73714ed4-a795-a7ae-7e93-36100ce7c242"
    pipeload(urls = urls, govcan = govcan, output = here::here(path, "raw"), large = TRUE)
  }
  # _________________________________________________________________________________________ #

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # NOTE: optional
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    sea_level <- sf::st_read(
      here::here(
        path, "raw",
        "CANCOAST_SEALEVELCHANGE_2006_2020_V1/CANCOAST_SEALEVELCHANGE_2006_2020_V1.shp"
      ),
      quiet = TRUE
    )
    material <- sf::st_read(
      here::here(path, "raw", "CANCOAST_MATERIAL_V2/CANCOAST_MATERIAL_V2.shp"),
      quiet = TRUE
    )
    # shoreline_v2 <- sf::st_read(
    #   glue("{path}raw/CANCOAST_SHORELINE_V2/CANCOAST_SHORELINE_V2.shp"),
    #   quiet = TRUE
    # )
    # shoreline_v3 <- sf::st_read(
    #   glue("{path}raw/CANCOAST_SHORELINE_V3/CANCOAST_SHORELINE_V3.shp"),
    #   quiet = TRUE
    # )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_crs = crs,
      pipeline_bbox = bbox,
      access = timestamp(),
      data_bbox = sf::st_bbox(sea_level),
      data_timespan = c(2010, 2011),
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
    sea_level <- dp_parameters(
      sea_level,
      crs = crs,
      bbox = bbox
    )
    material <- dp_parameters(
      material,
      crs = crs,
      bbox = bbox
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Sea level
    fm <- glue("{path}/{nm}_sea_level.geojson") # NOTE: not necessarily spatial data
    sf::st_write(sea_level, dsn = fm, quiet = TRUE) # for spatial data

    # Material
    fm <- glue("{path}/{nm}_material.geojson") # NOTE: not necessarily spatial data
    sf::st_write(material, dsn = fm, quiet = TRUE) # for spatial data

    # Metadata
    mt <- glue("{path}/{nm}.yaml")
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- glue("{path}/{nm}.bib")
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
