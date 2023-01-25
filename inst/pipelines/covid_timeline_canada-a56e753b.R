#' @eval get_name("a56e753b")
#'
#' @eval get_description("a56e753b")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: a56e753b
#'
#' @examples
#' \dontrun{
#' dp_a56e753b()
#' }
dp_a56e753b <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grd = here::here("data","grid","grid.tif"), integrate = TRUE, keep_raw = TRUE, ...) {
  uid <- "a56e753b"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_raw(uid)) {
    # This dataset is a special case, where data are continuously updated. 
    # Keep this in mind when running a workflow, as it may be relevant to download the most 
    # version of the dataset. I could add an extra parameter
    # Geographic data on health regions and provinces / territories from the github repo: 
    #   https://github.com/ccodwg/CovidTimelineCanada
    # Covid data from their API: 
    #   https://api.opencovid.ca/#/CovidTimelineCanada/get_timeseries_timeseries_get
    urls <- c(
      "https://api.opencovid.ca/timeseries?fmt=csv&geo=can",
      "https://api.opencovid.ca/timeseries?fmt=csv&geo=pt",
      "https://api.opencovid.ca/timeseries?fmt=csv&geo=hr",
      "https://github.com/ccodwg/CovidTimelineCanada/raw/main/geo/pt.csv",
      "https://github.com/ccodwg/CovidTimelineCanada/raw/main/geo/hr.csv",
      "https://github.com/ccodwg/CovidTimelineCanada/raw/main/geo/hr_wgs84.geojson",
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
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_format(uid)) {
    files <- c(
      "pt.csv",
      "hr.csv",
      "pt_wgs84.geojson",
      "hr_wgs84.geojson",
      "CovidTimelineCanada_can.csv",
      "CovidTimelineCanada_pt.csv",
      "CovidTimelineCanada_hr.csv"
    )
    filepaths <- here::here(path,"raw",files)
    dat <- lapply(filepaths, masterload)

    # Export
    fm <- here::here(path,"format",glue::glue("{nm}-{tools::file_path_sans_ext(files)}"))
    for(i in 1:length(fm)) masterwrite(dat[[i]], fm[i])
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
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Metadata & bibtex
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
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

  # Clean 
  clean_path(uid, keep_raw)
  # _________________________________________________________________________________________ #
}
