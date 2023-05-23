#' @eval get_name("6eefac0b")
#'
#' @eval get_description("6eefac0b")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 6eefac0b
#'
#' @examples
#' \dontrun{
#' dp_6eefac0b()
#' }
dp_6eefac0b <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, ...) {
  uid <- "6eefac0b"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_raw(uid)) {
    govcan <- get_pipeline(uid)$data_uuid
    pipeload(
      govcan = govcan,
      output = here::here(path, "raw"),
      large = FALSE
    )
    
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
    dat <- list()
    dat[[1]] <- masterload(here::here(path, "raw", "AL_TA_CA_2_152_MODIFIED_eng.shp"))
    dat[[2]] <- masterload(here::here(path, "raw", "AL_TA_CA_2_152_CONFIRMED_eng.shp"))
    
    dat <- dplyr::bind_rows(dat)
  
    # Subset data (if specified by user)
    # on.exit(sf::sf_use_s2(TRUE), add = TRUE)
    # sf::sf_use_s2(FALSE)
    dat <- lapply(dat, dp_parameters, bbox = bbox, timespan = timespan)
  
    # Export
    fm <- here::here(path,"format",nm)
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
  if (check_ingrid(uid) & ingrid) {
    # Import in grid
    dat <- importdat(uid, "format") |>
           lapply(function(x) {
             dplyr::mutate(x, aboriginal_lands_canada = 1) |>
             dplyr::select(aboriginal_lands_canada) |>
             stars::st_rasterize() |>
             masteringrid()
           })
        
    # Export 
    fm <- here::here(path,"ingrid",glue::glue("{tools::file_path_sans_ext(names(dat))}"))    
    masterwrite(dat[[1]], fm)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    meta <- load_metadata(path, nm) |>
    add_ingrid(
      ingrid = list(
        timestamp = timestamp(),
        description = c(
          "Aboriginal Lands, which include legislative boundaries of Indian Reserves, Land Claim Settlement Lands (lands created under Comprehensive Land Claims Process that do not or will not have Indian Reserve status under the Indian Act) and Indian Lands, were integrated in the study grid as presence absence data."
        ),
        files = list(
          filenames = basename(fm),
          names = glue::glue(
            glue::glue("Location of aboriginal lands")
          )          
        )
      )
    )
    masterwrite(meta, here::here(path, nm))
  }
  # _________________________________________________________________________________________ #

  # Clean 
  clean_path(uid)
  # _________________________________________________________________________________________ #
}
