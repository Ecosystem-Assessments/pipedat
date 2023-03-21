#' @eval get_name("7daa23ee")
#'
#' @eval get_description("7daa23ee")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 7daa23ee
#'
#' @examples
#' \dontrun{
#' dp_7daa23ee()
#' }
dp_7daa23ee <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, keep_raw = TRUE, ...) {
  uid <- "7daa23ee"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_raw(uid)) {    
    urls <- c(
      "https://www150.statcan.gc.ca/n1/en/pub/92-500-g/92-500-g2021001-eng.pdf?st=OgZb6M3u",
      "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/RNF-FRR/files-fichiers/lrnf000r21f_e.zip"      
    )

    # Load
    pipeload(
      urls = urls, 
      output = here::here(path, "raw"), 
      large = TRUE
    )
    
    # Rename pdf
    file.rename(
      from = here::here(path,"raw","92-500-g2021001-eng.pdf?st=OgZb6M3u"),
      to = here::here(path,"raw","92-500-g2021001-eng.pdf")
    )
  }
  # _________________________________________________________________________________________ #    
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_format(uid)) {
    # Files
    dat <- sf::st_read(
      here::here(path,"raw","lrnf000r21f_e.gdb"),
      layer = "lrnf000r21f_e",
      quiet = TRUE
    )

    # Subset data (if specified by user)
    # on.exit(sf::sf_use_s2(TRUE), add = TRUE)
    # sf::sf_use_s2(FALSE)
    # dat <- lapply(dat, dp_parameters, bbox = bbox, timespan = timespan)
    dat <- dp_parameters(dat, bbox)

  # Export
    fm <- here::here(path,"format",glue::glue("{nm}"))
    masterwrite(dat, fm)    
  } 
  # _________________________________________________________________________________________ #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Integrate data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Maybe at some point: https://michaelbcalles.netlify.app/post/rasterize-lines-in-r/
  # Rasterization of line segments, even with buffers, is not working well with all raster packages
  # Using vectors first, than transforming to rasters
  if (check_ingrid(uid) & ingrid) {
    # Import data 
    dat <- importdat(uid, "format")[[1]] 
    
    # Rasterize & import in grid
    bb <- sf::st_bbox(dat)
    rt <- raster::raster(
      xmn = bb$xmin, ymn = bb$ymin, 
      xmx = bb$xmax, ymx = bb$ymax, 
      crs = raster::projection(dat), 
      res = 1000
    )

    # Get points and add buffer around points. 
    # WARNING: This is imperfect, but may be necessary for a working version 
    grd <- stars::st_as_stars(rt) |>
           as.data.frame() |>
           sf::st_as_sf(coords = c("x","y"), crs = sf::st_crs(dat))

    # # Make vectorized grid
    # grd <- sf::st_make_grid(dat, cellsize = 1000) |>
    #        sf::st_as_sf()

    # Buffer roads
    roads <- dat |> 
             sf::st_cast("MULTILINESTRING") |>
             sf::st_make_valid() |>
             sf::st_simplify(preserveTopology = TRUE, dTolerance = 100) |>
             sf::st_buffer(dist = 2000) |>
             dplyr::select(RANK, geom)
    # sf::st_write(roads, dsn = "temp.gpkg", overwrite = TRUE)
              
    # Intersect with grid (binary)
    int <- sf::st_intersects(roads, grd)
    
    # Add to grid 
    iid <- sort(unique(unlist(int)))
    grd <- grd[iid, ] |>
           dplyr::select(geometry) |>
           sf::st_buffer(1000)
    
    # Import in grid
    r <- fasterize::fasterize(grd, rt) |> 
         stars::st_as_stars() |>
         masteringrid()

    # Export 
    masterwrite(r, here::here(path, "ingrid", glue::glue("{nm}-road_network")))
    
    # -----------------------------------
    # Calculate distance to closest road
    r <- stars::read_stars(here::here(path, "ingrid",glue::glue("{nm}-road_network.tif")))
    rd <- as.data.frame(r) |>
          dplyr::mutate(id = 1:dplyr::n()) |>
          dplyr::rename(roads = census_road_network_file_2021.7daa23ee.road_network.tif)
        
    # Fuction to calculate distances
    dist_calc <- function(from, to) {
      sf::st_distance(from, to) |>
      units::set_units("km") |>
      apply(MARGIN = 1, FUN = min)
    }
    
    # Use study grid to identify cells that are only terrestrial (if relevant)
    grd <- stars::read_stars(here::here("data","grid","grid.tif")) |>
           as.data.frame() |>
           dplyr::mutate(id = 1:dplyr::n()) |>
           tidyr::drop_na() |>
           dplyr::select(id)

    # Points with and without roads in grid
    idna <- is.na(rd$roads)
    noroads <- rd[idna, ] |> 
               dplyr::filter(id %in% grd$id) |>
               sf::st_as_sf(coords = c("x","y"), crs = sf::st_crs(r)) |>
               dplyr::select(id)
    yesroads <- rd[!idna, ] |>
                dplyr::select(id) |>
                dplyr::mutate(distance_roads = 0)
    
    # Rasterized roads as polygons to ease calculations 
    pol <- sf::st_as_sf(r, as_points = FALSE, merge = TRUE)
        
    # Calculate distances
    distrd <- dist_calc(from = noroads, to = pol)
    noroads$distance_roads <- distrd
    noroads <- sf::st_drop_geometry(noroads)
    
    # Add to grid and rasterize 
    dist_rd <- dplyr::bind_rows(noroads, yesroads)
    rd <- dplyr::left_join(rd, dist_rd, by = "id") |>
          dplyr::select(x,y,distance_roads) |>
          stars::st_as_stars(coords = c("x","y"),  crs = sf::st_crs(r))
    
    # Export 
    masterwrite(rd, here::here(path, "ingrid", glue::glue("{nm}-distance_to_road_network")))
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
