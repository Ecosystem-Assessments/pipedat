#' @eval get_name("8671c3e4")
#'
#' @eval get_description("8671c3e4")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 8671c3e4
#'
#' @examples
#' \dontrun{
#' dp_8671c3e4()
#' }
dp_8671c3e4 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, keep_raw = TRUE, census_geo_8671c3e4 = "division", ...) {
  uid <- "8671c3e4"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_raw(uid)) {
    pipedat("37563350") # Census Profile 2021, Census of Population

    if ("division" %in% census_geo_8671c3e4 ) {
      pipedat("288ca300") # Census cartographic division boundary files 2021
    }

    if ("subdivision" %in% census_geo_8671c3e4) {    
      pipedat("5e4be996") # Census cartographic subdivision boundary files 2021
    }
  }
  # _________________________________________________________________________________________ #    
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_format(uid)) {
    # Statistics Canada census divisions & subdivisions
    if ("division" %in% census_geo_8671c3e4 ) {
      div <- importdat("288ca300", "format")[[1]] |>
             dplyr::select(DGUID)
    }

    if ("subdivision" %in% census_geo_8671c3e4) {    
      sdv <- importdat("5e4be996", "format")[[1]] |>
             dplyr::select(DGUID)
    }

    # Census 2021 housing suitability
    census <- importdat("37563350", "format")[[1]] 

    # List to store data 
    dat <- list()
    # ---------------------------------------------------------
    # Unemployment rate
    iid <- census$CHARACTERISTIC_NAME == "Unemployment rate"
    dat[[1]] <- census[iid,] |>
      dplyr::select(DGUID, unemployment_rate = C1_COUNT_TOTAL)

    # ---------------------------------------------------------
    # Gini index
    iid <- census$CHARACTERISTIC_NAME == "Gini index on adjusted household total income"
    dat[[2]] <- census[iid,] |>
           dplyr::select(DGUID, gini_index_adj_household_total_income = C1_COUNT_TOTAL)

    # ---------------------------------------------------------
    # P90/P10 ratio
    iid <- census$CHARACTERISTIC_NAME == "P90/P10 ratio on adjusted household after-tax income"
    dat[[3]] <- census[iid,] |>
           dplyr::select(DGUID, p90p10_ratio_ajd_household_aftertax_income = C1_COUNT_TOTAL)

    # ---------------------------------------------------------
    # Low-income measure after tax (LIM-AT)
    # An income threshold substantially below what is typical in society.
    iid <- census$CHARACTERISTIC_NAME == 
      "Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)"
    dat[[4]] <- census[iid,] |>
           dplyr::select(DGUID, low_income_measure_aftertax_percent = C1_COUNT_TOTAL)

    # ---------------------------------------------------------
    # Low-income cut-offs after tax (LICO-AT)
    # An income threshold below which a family will devote a much larger share of its income than
    # the average family on the necessities of food, shelter, and clothing.
    iid <- census$CHARACTERISTIC_NAME == 
      "Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)"
    dat[[5]] <- census[iid,] |>
           dplyr::select(DGUID, low_income_cutoffs_aftertax_percent = C1_COUNT_TOTAL)

    # ---------------------------------------------------------
    # Percent indigenous identity
    iid <- census$CHARACTERISTIC_NAME == "Indigenous identity"
    dat[[6]] <- census[iid,] |>
           dplyr::select(DGUID, percent_indigenous_identity = C10_RATE_TOTAL)

    # ---------------------------------------------------------
    # Percent children in one-parent family
    iid <- census$CHARACTERISTIC_NAME == "In a one-parent family"
    dat[[7]] <- census[iid,] |>
           dplyr::select(DGUID, percent_children_one_parent_family = C10_RATE_TOTAL)

    # ---------------------------------------------------------
    # Percent parents in one-parent family
    iid <- census$CHARACTERISTIC_NAME == "Parents in one-parent families"
    dat[[8]] <- census[iid,] |>
           dplyr::select(DGUID, percent_parent_one_parent_family = C10_RATE_TOTAL)

    # ---------------------------------------------------------
    # Percent with no certificate, diploma or degree, population 25-64 years old
    # Only ID is sufficient, but I want to keep trace of name for code readability
    # https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?LANG=E&GENDERlist=1,2,3&STATISTIClist=4&HEADERlist=37&SearchText=Canada&DGUIDlist=2021A000011124
    iid <- census$CHARACTERISTIC_NAME == "No certificate, diploma or degree" & 
           census$CHARACTERISTIC_ID == "2015" 
    dat[[9]] <- census[iid,] |>
           dplyr::select(DGUID, percent_no_certificate_diploma_degree = C10_RATE_TOTAL)

    # ---------------------------------------------------------
    # Percent of total income composed of government transfers in 2020
    # Only ID is sufficient, but I want to keep trace of name for code readability
    iid <- census$CHARACTERISTIC_NAME == "Government transfers (%)" &
           census$CHARACTERISTIC_ID == "151" 
    dat[[10]] <- census[iid,] |>
           dplyr::select(DGUID, percent_government_transfers = C1_COUNT_TOTAL)

    # ---------------------------------------------------------
    # Join to geographies
    if ("division" %in% census_geo_8671c3e4 ) {
      for(i in 1:length(dat)) {
        div <- dplyr::left_join(div, dat[[i]], by = "DGUID")
      }
    }

    if ("subdivision" %in% census_geo_8671c3e4) {    
      for(i in 1:length(dat)) {
        sdv <- dplyr::left_join(sdv, dat[[i]], by = "DGUID")
      }
    }

    # ---------------------------------------------------------
    # Export
    if ("division" %in% census_geo_8671c3e4 ) {
      fm <- here::here(path,"format",glue::glue("{nm}-ceusus_division"))
      masterwrite(div, fm)    
    }

    if ("subdivision" %in% census_geo_8671c3e4) {    
      fm <- here::here(path,"format",glue::glue("{nm}-ceusus_subdivision"))
      masterwrite(sdv, fm)    
    }
  } 
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Integrate data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_ingrid(uid) & ingrid) {
    census_ingrid <- function(dat, suffix) {
      datNames <- colnames(dat)[-ncol(dat)]
      bb <- sf::st_bbox(dat)
      rt <- raster::raster(
        xmn = bb$xmin, ymn = bb$ymin, 
        xmx = bb$xmax, ymx = bb$ymax, 
        crs = sf::st_crs(dat)$epsg, 
        res = 500
      )
      
      
      r <- list()
      for(i in 1:length(datNames)) {
        r[[i]] <- fasterize::fasterize(dat[,i], rt, field = datNames[i], fun = "max") |>
                  stars::st_as_stars() |>
                  masteringrid()
      }

      # Export 
      fm <- glue::glue("{nm}-{suffix}-{datNames}")
      for(i in 1:length(r)) masterwrite(r[[i]], here::here(path, "ingrid", fm[i]))      
    }
    
    # Get formatted data and add to study grid
    if ("division" %in% census_geo_8671c3e4 ) {
      iid <- "canadian_vulnerabilities_census-8671c3e4-ceusus_division.gpkg"
      importdat("8671c3e4", "format")[[iid]] |>
      dplyr::select(-DGUID) |>
      census_ingrid("census_division")
    }

    if ("subdivision" %in% census_geo_8671c3e4) {    
      iid <- "canadian_vulnerabilities_census-8671c3e4-ceusus_subdivision.gpkg"
      importdat("8671c3e4", "format")[[iid]] |>
      dplyr::select(-DGUID) |>
      census_ingrid("census_subdivision")
    }
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
