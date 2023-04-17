#' @eval get_name("004b3c51")
#'
#' @eval get_description("004b3c51")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 004b3c51
#'
#' @examples
#' \dontrun{
#' dp_004b3c51()
#' }
dp_004b3c51 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, keep_raw = TRUE, ...) {
  uid <- "004b3c51"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_raw(uid)) {
    # If the data is downloaded from online sources
    urls <- c(
      "https://geo.vliz.be/geoserver/wfs?request=getfeature&service=wfs&version=1.0.0&typename=MarineRegions:eez&outputformat=SHAPE-ZIP&filter=%3CPropertyIsEqualTo%3E%3CPropertyName%3Emrgid%3C%2FPropertyName%3E%3CLiteral%3E8493%3C%2FLiteral%3E%3C%2FPropertyIsEqualTo%3E"
    )
    pipeload(
      urls = urls, 
      output = here::here(path, "raw"), 
      large = FALSE
    )
    file.rename(
      dir(here::here(path,"raw"), full.names = TRUE),
      here::here(path,"raw","eez.zip")
    )
    utils::unzip(here::here(path,"raw","eez.zip"), exdir = here::here(path, "raw"))
    unlink(here::here(path,"raw","eez.zip"))
  }
  # _________________________________________________________________________________________ #    
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_format(uid)) {
    out <- here::here(path, "format")
    
    # Files
    dat <- masterload(here::here(path,"raw","eez.shp"))
  
    # Subset data (if specified by user)
    dat <- dp_parameters(dat, bbox, timespan)

    # Export
    fm <- here::here(out,glue::glue("{nm}"))
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
  # Metadata & bibtex & code
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
