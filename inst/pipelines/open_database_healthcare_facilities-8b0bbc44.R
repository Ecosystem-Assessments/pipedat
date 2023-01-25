#' @eval get_name("8b0bbc44")
#'
#' @eval get_description("8b0bbc44")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 8b0bbc44
#'
#' @examples
#' \dontrun{
#' dp_8b0bbc44()
#' }
dp_8b0bbc44 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grd = here::here("data","grid","grid.tif"), integrate = TRUE, keep_raw = TRUE, ...) {
  uid <- "8b0bbc44"
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
  }
  # _________________________________________________________________________________________ #    
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_format(uid)) {
    # Import 
    dat <- masterload(here::here(path,"raw","odhf_v1.csv")) 

    # Correct data with positive longitudes (should all be negative)
    dat$longitude <- ifelse(dat$longitude > 0, -dat$longitude, dat$longitude)
    
    # Manually modify a single coordinate 
    # I noticed this one, but there might be more, and there are also many NAs in the dataset
    index <- dat$index == 7485
    dat$latitude[index] <- 46.818383118363435
    dat$longitude[index] <- -71.1562927
  
    # Modify odhf facility type 
    dat$odhf_facility_type <- gsub(
      "nursing and residential care facilities",
      "Nursing and residential care facilities", 
      dat$odhf_facility_type
    )
    
    # Create a spatial data as well as the table which includes NA locations
    spat <- dplyr::filter(dat, !is.na(latitude)) |>
            sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

    # Subset data (if specified by user)
    spat <- dp_parameters(spat, bbox)


    # Export
    fm <- here::here(path,"format",glue::glue("{nm}"))   
    masterwrite(dat, fm)
    masterwrite(spat, fm)
  } 
  # _________________________________________________________________________________________ #

  # # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # # Integrate data 
  # # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # if (check_integrated(uid)) {
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
