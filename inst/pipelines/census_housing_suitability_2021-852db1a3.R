#' @eval get_name("852db1a3")
#'
#' @eval get_description("852db1a3")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 852db1a3
#'
#' @examples
#' \dontrun{
#' dp_852db1a3()
#' }
dp_852db1a3 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, keep_raw = TRUE, ...) {
  uid <- "852db1a3"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_raw(uid)) {
    urls <- "https://www150.statcan.gc.ca/n1/tbl/csv/98100237-eng.zip"
    pipeload(urls = urls, output = here::here(path, "raw"), large = TRUE)
    unlink(here::here(path,"raw","98100237-eng.zip"))
  }
  # _________________________________________________________________________________________ #    
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_format(uid)) {
    dat <- masterload(here::here(path,"raw","98100237.csv"))

    # Export
    fm <- here::here(path,"format",glue::glue("{nm}"))
    masterwrite(dat, fm)    
  } 
  # _________________________________________________________________________________________ #

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
