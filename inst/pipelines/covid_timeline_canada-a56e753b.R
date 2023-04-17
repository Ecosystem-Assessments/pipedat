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
dp_a56e753b <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, keep_raw = TRUE, ...) {
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

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Integrate data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_ingrid(uid) & ingrid) {
    dat <- importdat(uid, "format")
    hr <- dat[["covid_timeline_canada-a56e753b-hr_wgs84.gpkg"]] |> 
          dplyr::mutate(hruid = as.character(hruid)) |>
          dplyr::select(hruid)
    
    hrpop <- dat[["covid_timeline_canada-a56e753b-hr.csv"]] |>
             dplyr::select(hruid, pop) |>
             dplyr::mutate(hruid = as.character(hruid))
             
    ### NOTE: First go at cases and deaths, just take the cumulative total 
    cases <- dat[["covid_timeline_canada-a56e753b-CovidTimelineCanada_hr.csv"]] |>
             dplyr::group_by(name, sub_region_1) |>
             dplyr::summarize(value = max(value)) |>
             dplyr::ungroup()
    iid <- cases$name == "cases"
    deaths <- cases[!iid, ] |>
              dplyr::select(hruid = sub_region_1, deaths = value) |>
              dplyr::mutate(hruid = as.character(hruid))
    cases <- cases[iid, ] |>
             dplyr::select(hruid = sub_region_1, cases = value) |>
             dplyr::mutate(hruid = as.character(hruid))

    # Join to spatial data and divide by total population
    cases <- dplyr::left_join(hr, cases, by = "hruid") |> 
             dplyr::left_join(hrpop, by = "hruid") |>
             dplyr::mutate(cases_pop_prop = cases / pop) |>
             dplyr::select(cases_pop_prop) |>
             stars::st_rasterize()
    deaths <- dplyr::left_join(hr, deaths, by = "hruid") |> 
             dplyr::left_join(hrpop, by = "hruid") |>
             dplyr::mutate(deaths_pop_prop = deaths / pop) |>
             dplyr::select(deaths_pop_prop) |>
             stars::st_rasterize()
    
    # Import in grid
    cases <- masteringrid(cases)
    deaths <- masteringrid(deaths)
  
    # Export 
    masterwrite(cases, here::here(path, "ingrid", glue::glue("{nm}-cases_population_proportion")))
    masterwrite(deaths, here::here(path, "ingrid", glue::glue("{nm}-deaths_population_proportion")))
  }
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
