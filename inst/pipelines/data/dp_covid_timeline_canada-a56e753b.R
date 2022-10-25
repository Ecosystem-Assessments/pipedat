#' @eval get_name("a56e753b")
#'
#' @eval get_description("a56e753b")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: a56e753b
#'
#' @examples
#' \dontrun{
#' dp_a56e753b()
#' }
dp_a56e753b <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "a56e753b"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

    
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # This dataset is a special case, where data are continuously updated. 
  # Keep this in mind when running a workflow, as it may be relevant to download the most 
  # version of the dataset. I could add an extra parameter
  # Geographic data on health regions and provinces / territories from the github repo: 
  #   https://github.com/ccodwg/CovidTimelineCanada
  # Covid data from their API: 
  #   https://api.opencovid.ca/#/CovidTimelineCanada/get_timeseries_timeseries_get
  if (!exist$raw) {
    urls <- c(
      "https://api.opencovid.ca/timeseries?fmt=csv&geo=can",
      "https://api.opencovid.ca/timeseries?fmt=csv&geo=pt",
      "https://api.opencovid.ca/timeseries?fmt=csv&geo=hr",
      "https://github.com/ccodwg/CovidTimelineCanada/raw/main/geo/pt.csv",
      "https://github.com/ccodwg/CovidTimelineCanada/raw/main/geo/health_regions.csv",
      "https://github.com/ccodwg/CovidTimelineCanada/raw/main/geo/health_regions_wgs84.geojson",
      "https://github.com/ccodwg/CovidTimelineCanada/raw/main/geo/pt_wgs84.geojson"
    )
  
    # Load
    pipeload(
      urls = urls, 
      output = here::here(path, "raw"), 
      large = FALSE
    )
    
    # Rename 
    from <- c(
      "timeseries?fmt=csv&geo=can",
      "timeseries?fmt=csv&geo=pt", 
      "timeseries?fmt=csv&geo=hr"
    )
    to <- c(
      "CovidTimelineCanada_can.csv",
      "CovidTimelineCanada_pt.csv",
      "CovidTimelineCanada_hr.csv"
    )
    for(i in 1:3) file.rename(here::here(path,"raw",from[i]), here::here(path,"raw",to[i]))
  }
  # _________________________________________________________________________________________ #
    
  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Example for data that needs to be locally available
    files <- c(
      "pt.csv",
      "health_regions.csv",
      "pt_wgs84.geojson",
      "health_regions_wgs84.geojson",
      "CovidTimelineCanada_can.csv",
      "CovidTimelineCanada_pt.csv",
      "CovidTimelineCanada_hr.csv"
    )
    filepaths <- here::here(path,"raw",files)
    dat <- lapply(filepaths, masterload)
    # _________________________________________________________________________________________ #
    
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      # pipeline_bbox = bbox, 
      # pipeline_bbox_crs = bbox_crs, 
      # pipeline_timespan = timespan, 
      access = timestamp(), 
      # data_bbox = sf::st_bbox(dat), 
      data_timespan = 2020:2022
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #
    
    # # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # # APPLY SUBSETS SPECIFIED BY USER
    # # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # # on.exit(sf::sf_use_s2(TRUE), add = TRUE)
    # # sf::sf_use_s2(FALSE)
    # # dat <- lapply(dat, dp_parameters, bbox = bbox, bbox_crs = bbox_crs, timespan = timespan)
    # dat <- dp_parameters(
    #   dat,
    #   bbox = bbox,
    #   bbox_crs = bbox_crs,
    #   timespan = timespan
    # )
    # # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT 
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data   
    fm <- here::here(path,glue::glue("{nm}-{tools::file_path_sans_ext(files)}"))
    for(i in 1:length(fm)) masterwrite(dat[[i]], fm[i])
    
    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)  
    # _________________________________________________________________________________________ #
  } #if exist clean, don't run again
}
