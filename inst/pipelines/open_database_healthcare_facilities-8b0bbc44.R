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
dp_8b0bbc44 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, ...) {
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

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Integrate data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_ingrid(uid) & ingrid) {
    # Health care facilities 
    health <- importdat(uid, "format")[[2]]
    
    # # WARNING: will have to do this again, I unfortunately deleted the work we did... 
    # # Type of healthcare facilities 
    # type <- sf::st_drop_geometry(health) |>
    #         dplyr::select(odhf_facility_type, source_facility_type) |>
    #         dplyr::distinct()
    # # Figure out how to reclassify so that it makes sense
    # health <- dplyr::left_join(health, type, by = "source_facility_type")
    # critical <- health[health$type %in% "critical",]
    # longterm <- health[health$type %in% c("long-term","diagnostics","cnbnl"),]
    
    # WARNING: For now...
    critical <- c("Hospitals","Ambulatory health care services")
    critical <- health[health$odhf_facility_type %in% critical, ] |>
                dplyr::select(odhf_facility_type)
    longterm <- "Nursing and residential care facilities"
    longterm <- health[health$odhf_facility_type %in% longterm, ] |>
                dplyr::select(odhf_facility_type)
    
    # Calculate distances 
    # max_dist is not used for now
    dist_calc <- function(from, to, max_dist = NULL) {
      dat <- sf::st_distance(from, to) |>
             units::set_units("km") |>
             apply(MARGIN = 1, FUN = min)
    
      ## Cap to max dist
      # max_dist <- max(dat, na.rm = TRUE)
      # dat <- ifelse(dat > max_dist, max_dist, dat) 
    
      # Transform as an index between 0 and 1, with 1 being the farthest, and 0 the closest
      # dat <- dat/max_dist
      dat
    }
    
    # Grid 
    grd <- stars::read_stars(here::here("data","grid","grid.tif"))
    
    # Grid as points to measure distances 
    grd_pts <- as.data.frame(grd) |>
               dplyr::mutate(id = 1:dplyr::n()) 
    
    # Points data 
    pts <- tidyr::drop_na(grd_pts) |>
           dplyr::select(x,y,id) |>
           sf::st_as_sf(coords = c("x","y"), crs = 4326) 

    # Distances 
    pts$critical <- dist_calc(pts, critical)
    pts$longterm <- dist_calc(pts, longterm)
    
    # To raster again
    pts <- sf::st_drop_geometry(pts)
    grd_pts <- dplyr::left_join(grd_pts, pts, by = "id")
    critical <- dplyr::mutate(grd, critical = grd_pts$critical) |>
                dplyr::select(critical)
    longterm <- dplyr::mutate(grd, longterm = grd_pts$longterm) |>
                dplyr::select(longterm)
    
    # Export 
    masterwrite(critical, here::here(path, "ingrid", glue::glue("{nm}-critical")))
    masterwrite(longterm, here::here(path, "ingrid", glue::glue("{nm}-longterm")))
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
  clean_path(uid)
  # _________________________________________________________________________________________ #
}
