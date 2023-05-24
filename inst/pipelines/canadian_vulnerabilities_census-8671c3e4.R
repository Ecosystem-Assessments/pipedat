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
dp_8671c3e4 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, census_geo_8671c3e4 = "division", ...) {
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
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    if (!file.exists(here::here(path, glue::glue("{nm}.yaml")))) {
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

    # Census 2021
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
    files <- list()
    # Export
    if ("division" %in% census_geo_8671c3e4 ) {
      files$fmdiv <- here::here(path,"format",glue::glue("{nm}-census_division"))
      masterwrite(div, files$fmdiv)    
    }

    if ("subdivision" %in% census_geo_8671c3e4) {    
      files$fmsub <- here::here(path,"format",glue::glue("{nm}-census_subdivision"))
      masterwrite(sdv, files$fmsub)
    }
    files <- basename(unlist(files))
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    statement <- c(
      "census cartographic divisions and subdivisions boundary files for 2021 [@statisticscanada2022f; @statisticscanada2022]",
      "census cartographic divisions boundary files for 2021 [@statisticscanada2022f; @statisticscanada2022]",
      "census cartographic subdivisions boundary files for 2021 [@statisticscanada2022f; @statisticscanada2022]"
    )
    geos <- dplyr::case_when(
      "division" %in% census_geo_8671c3e4 &
      "subdivision" %in% census_geo_8671c3e4 ~ statement[1],
      census_geo_8671c3e4 == "division" ~ statement[2],
      census_geo_8671c3e4 == "subdivision" ~ statement[3]
    )      
    meta <- load_metadata(path, nm) |>
    add_format( 
      format = list(
        timestamp = timestamp(),
        description = glue::glue(
          'Data from the 2021 Census of Population [@statisticscanada2021a] was used to select relevant population indicators as proxies of social vulnerabilities. The indicators were then joined to the {geos} and subsequently integrated in the study grid. The selected indicators are:
  
          - ***Gini index on adjusted household total income***: *The Gini coefficient is a number between zero and one that measures the relative degree of inequality in the distribution of income. The coefficient would register zero (minimum inequality) for a population in which each person received exactly the same adjusted household income and it would register a coefficient of one (maximum inequality) if one person received all the adjusted household income and the rest received none. Even though a single Gini coefficient value has no simple interpretation, comparisons of the level over time or between populations are very straightforward: the higher the coefficient, the higher the inequality of the distribution.*
          - ***P90/P10 ratio on adjusted household after-tax incom***: *The P90/P10 ratio is a measure of inequality. It is the ratio of the 90th and the 10th percentile of the adjusted household after-tax income. The 90th percentile means 90% of the population has income that falls below this threshold. The 10th percentile means 10% of the population has income that falls below this threshold.*
          - ***Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)***: *The Low‑income measure, after tax, refers to a fixed percentage (50%) of median adjusted after‑tax income of private households. The household after‑tax income is adjusted by an equivalence scale to take economies of scale into account. This adjustment for different household sizes reflects the fact that a household\'s needs increase, but at a decreasing rate, as the number of members increases.*
          - ***Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)***: *The Low‑income cut‑offs, after tax refer to income thresholds, defined using 1992 expenditure data, below which economic families or persons not in economic families would likely have devoted a larger share of their after‑tax income than average to the necessities of food, shelter and clothing. More specifically, the thresholds represented income levels at which these families or persons were expected to spend 20 percentage points or more of their after‑tax income than average on food, shelter and clothing. These thresholds have been adjusted to current dollars using the all‑items Consumer Price Index (CPI).*
          - ***Indigenous identity***: *Indigenous identity refers to whether the person identified with the Indigenous peoples of Canada. This includes those who identify as First Nations (North American Indian), Métis and/or Inuk (Inuit), and/or those who report being Registered or Treaty Indians (that is, registered under the Indian Act of Canada), and/or those who have membership in a First Nation or Indian band. Aboriginal peoples of Canada (referred to here as Indigenous peoples) are defined in the Constitution Act, 1982, Section 35 (2) as including the Indian, Inuit and Métis peoples of Canada.*
          - ***In a one-parent family***: *Percent children living in one-parent family*
          - ***Parents in one-parent families***: *Percent parent in one-parent family
          - ***No certificate, diploma or degree***: *Percent population with no certificate, diploma or degree, population 25-64 years old*
          - ***Government transfers (%)***: *Percent of total income composed of government transfers in 2020, corresponding to all cash benefits received from federal, provincial, territorial or municipal governments during the reference period.*'
        ),
        filenames = files
      )
    )
    masterwrite(meta, here::here(path, nm))
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
      iid <- "canadian_vulnerabilities_census-8671c3e4-census_division.gpkg"
      importdat("8671c3e4", "format")[[iid]] |>
      dplyr::select(-DGUID) |>
      census_ingrid("census_division")
    }

    if ("subdivision" %in% census_geo_8671c3e4) {    
      iid <- "canadian_vulnerabilities_census-8671c3e4-census_subdivision.gpkg"
      importdat("8671c3e4", "format")[[iid]] |>
      dplyr::select(-DGUID) |>
      census_ingrid("census_subdivision")
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    geos <- dplyr::case_when(
      "division" %in% census_geo_8671c3e4 &
      "subdivision" %in% census_geo_8671c3e4 ~ statement[1],
      census_geo_8671c3e4 == "division" ~ statement[2],
      census_geo_8671c3e4 == "subdivision" ~ statement[3]
    )      
    indicators <- data.frame(
      names = c(
        "Gini index on adjusted household total income",
        "P90/P10 ratio on adjusted household after-tax income",
        "Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)",
        "Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)",
        "Indigenous identity",
        "In a one-parent family",
        "Parents in one-parent families",
        "No certificate, diploma or degree",
        "Government transfers (%)"        
      ),
      files = c(
        "gini_index_adj_household_total_income",
        "p90p10_ratio_ajd_household_aftertax_income",
        "low_income_measure_aftertax_percent",
        "low_income_cutoffs_aftertax_percent",
        "percent_indigenous_identity",
        "percent_children_one_parent_family",
        "percent_parent_one_parent_family",
        "percent_no_certificate_diploma_degree",
        "percent_government_transfers"        
      )
    )
    filenames <- glue::glue('{nm}-census_{sort(census_geo_8671c3e4)}')
    meta <- load_metadata(path, nm) |>
    add_ingrid(
      ingrid = list(
        timestamp = timestamp(),
        description = meta$format$description,
        files = list(
          filenames = glue::glue("{rep(filenames, each = nrow(indicators))}-{rep(indicators$files, length(filenames))}"),
          names = rep(indicators$names, length(filenames))
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
