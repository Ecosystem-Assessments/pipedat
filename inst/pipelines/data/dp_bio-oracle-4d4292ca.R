#' @eval get_name("4d4292ca")
#'
#' @eval get_description("4d4292ca")
#'
#' @eval dp_params()
#' @param biooracle_layers character, name of bio-oracle layers to download. Use `sdmpredictors::list_layers()` to see list of layers available, but be aware that the list also contains data from MARSPEC and BioClim.

#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 4d4292ca
#'
#' @examples
#' \dontrun{
#' dp_4d4292ca()
#' }
dp_4d4292ca <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, biooracle_layers = "BO_bathymean", ...) {
  # Output folders and other objects used
  uid <- "4d4292ca"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_output(uid)
    
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  files <- dir(here::here(path,"raw"))
  if (!length(files) > 0) {
    # NOTE: This pipeline is a bit different from other pipelines 
    # 1. here because there are so many layers in the sdmpredictors package
    # 2. there is already a package that provides access to the data: `sdmpredictors`
    #    we are thus directly using this package as a dependency and providing users the 
    #    ability to load all Bio-ORACLE data available through that package
    # WARNING: This means that `importdat` will not work with this particular pipeline
    dat <- sdmpredictors::load_layers(
      biooracle_layers,
      rasterstack=FALSE,
      datadir = here::here(path, "raw")
    )
  }
  # _________________________________________________________________________________________ #
  
  files <- dir(here::here(path), pattern = ".tif$")  
  if (!length(files) > 0) {
     # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Unzip files
    output <- here::here(path,"raw","data")
    zipfiles <- dir(here::here(path,"raw"), pattern = ".zip", full.names = TRUE)
    lapply(zipfiles, function(x) utils::unzip(x, exdir = output))
    
    # Import data 
    tifs <- dir(output, full.names = TRUE, pattern = ".tif$")
    dat <- lapply(tifs, stars::read_stars, proxy = TRUE)
      
    # _________________________________________________________________________________________ #
    
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox, 
      pipeline_bbox_crs = bbox_crs, 
      access = timestamp(), 
      data_bbox = sf::st_bbox(dat[[1]])
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #
    
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # APPLY SUBSETS SPECIFIED BY USER
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # on.exit(sf::sf_use_s2(TRUE), add = TRUE)
    # sf::sf_use_s2(FALSE)
    dat <- lapply(dat, dp_parameters, bbox = bbox, bbox_crs = bbox_crs)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT 
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data 
    datnames <- lapply(dat, names) |>
                unlist() |>
                tools::file_path_sans_ext()
    fm <- here::here(path,glue::glue("{nm}-{datnames}"))
    for(i in 1:length(dat)) masterwrite(dat[[i]], fm[i])
    unlink(output, recursive = TRUE)
    
    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)  
    # _________________________________________________________________________________________ #
  } #if exist clean, don't run again
}
