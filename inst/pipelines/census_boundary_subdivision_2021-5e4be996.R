#' @eval get_name("5e4be996")
#'
#' @eval get_description("5e4be996")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 5e4be996
#'
#' @examples
#' \dontrun{
#' dp_5e4be996()
#' }
dp_5e4be996 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, keep_raw = TRUE, ...) {
  uid <- "5e4be996"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_raw(uid)) {
    urls <- "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lcsd000b21a_e.zip"    
    pipeload(
      urls = urls, 
      output = here::here(path, "raw"), 
      large = TRUE
    )
  }
  # _________________________________________________________________________________________ #    
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_format(uid)) {
    zipfile <- here::here(path, "raw", "lcsd000b21a_e.zip")
    utils::unzip(zipfile, exdir = here::here(path, "raw"))
    dat <- sf::st_read(
      here::here(path, "raw", "lcsd000b21a_e.shp"),
      quiet = TRUE
    ) |>
      sf::st_make_valid()
    unlink(zipfile)
    
    # Subset data (if specified by user)
    on.exit(sf::sf_use_s2(TRUE), add = TRUE)
    sf::sf_use_s2(FALSE)
    dat <- dp_parameters(dat, bbox = bbox) 

    # Export
    fm <- here::here(path,"format",glue::glue("{nm}"))
    masterwrite(dat, fm)    
  } 
  # _________________________________________________________________________________________ #

  # # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # # Integrate data 
  # # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # if (check_ingrid(uid) & ingrid) {
  #   # Import in grid
  #   dat <- masteringrid(dat)
  # 
  #   # Export 
  #   masterwrite(dat, here::here(path, "integrated", nm))
  # }
  # # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Metadata & bibtex
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Metadata
  meta <- get_metadata(
    pipeline_type = "data",
    pipeline_id = uid,
    pipeline_bbox = bbox, 
    pipeline_timespan = timespan, 
    access = timestamp()
  )
    
  # bibtex
  bib <- get_bib(uid)

  # Export
  mt <- here::here(path, nm)
  masterwrite(meta, mt)
  masterwrite(bib, mt)  
  write_pipeline(uid)

  # Clean 
  clean_path(uid, keep_raw)
  # _________________________________________________________________________________________ #
}
