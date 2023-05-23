#' @eval get_name("7fe284e4")
#'
#' @eval get_description("7fe284e4")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 7fe284e4
#'
#' @examples
#' \dontrun{
#' dp_7fe284e4()
#' }
dp_7fe284e4 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, ...) {
  uid <- "7fe284e4"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_raw(uid)) {
    "https://native-land.ca/api/index.php?maps=territories" |>
      geojsonsf::geojson_sf() |>
      sf::st_zm(drop = TRUE, what = "ZM") |>
      sf::st_write(here::here(path, "raw", "native_land_digital.geojson"), delete_dsn = TRUE)
      
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Metadata
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      access = timestamp()
    )
    
    # bibtex
    bib <- get_bib(uid)

    # Export
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)  
    write_pipeline(uid)
  }
  # _________________________________________________________________________________________ #    
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_format(uid)) {
    dat <- masterload(here::here(path, "raw", "native_land_digital.geojson"))
    
    # Subset data (if specified by user)
    # on.exit(sf::sf_use_s2(TRUE), add = TRUE)
    # sf::sf_use_s2(FALSE)
    # dat <- lapply(dat, dp_parameters, bbox = bbox, timespan = timespan)
    dat <- dp_parameters(dat, bbox, timespan)

    # Export
    fm <- here::here(path,"format",glue::glue("{nm}"))
    masterwrite(dat, fm)   
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    meta <- load_metadata(path, nm) |>
    add_format( 
      format = list(
        timestamp = timestamp(),
        description = "No modifications applied to the data; simple export of raw data.",
        filenames = basename(fm)
      )
    )
    masterwrite(meta, here::here(path, nm)) 
  } 
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Integrate data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # if (check_ingrid(uid) & ingrid) {
  #   dat <- importdat(uid, "format")[[1]] |>
  #          stars::st_rasterize() |>
  #          masteringrid()
  # 
  #   # Export 
  #   masterwrite(dat, here::here(path, "ingrid", nm))
  # }
  # _________________________________________________________________________________________ #

  # Clean 
  clean_path(uid)
  # _________________________________________________________________________________________ #
}
