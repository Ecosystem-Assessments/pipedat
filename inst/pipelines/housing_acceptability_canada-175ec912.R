#' @eval get_name("175ec912")
#'
#' @eval get_description("175ec912")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 175ec912
#'
#' @examples
#' \dontrun{
#' dp_175ec912()
#' }
dp_175ec912 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, ...) {
  uid <- "175ec912"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_raw(uid)) {
    pipedat <- c(
      "852db1a3", # Census 2021 housing suitability
      "b48b01d6", # Census 2021 dwelling condition
      "f4abec86", # Census 2021 acceptable housing
      # "5e4be996" # Census cartographic subdivision boundary files 2021
      "288ca300" # Census cartographic division boundary files 2021
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
    ## Statistics Canada census subdivisions
    sc2021 <- importdat("288ca300", "format")[[1]] |>
              dplyr::select(DGUID)

    ## Census 2021 housing suitability
    hs <- importdat("852db1a3", "format")[[1]] 
    hs <- dplyr::filter(hs, DGUID %in% sc2021$DGUID)
    hs <- dplyr::filter(
      hs,
      `Housing suitability (3)` != "Total - Housing suitability" &
      `Number of persons per room (3)` == "Total - Number of persons per room" &
      `Statistics (3C)` == "Number of private households" &
      `Tenure (4)` == "Total - Tenure" &
      `Number of rooms and number of bedrooms (12)` == 
        "Total - Number of rooms and number of bedrooms"
    ) |>
    dplyr::select(
      DGUID,
      housing_suitability =`Housing suitability (3)`,
      household_size = `Household size (8):Total - Household size[1]`,
      average_household_size = `Household size (8):Average household size[8]`
    ) |>
    tidyr::pivot_wider(
      id_cols = DGUID,
      names_from = housing_suitability,
      values_from = c(household_size, average_household_size)
    ) |>
    dplyr::rename(
      household_size_suitable = household_size_Suitable,
      household_size_not_suitable = `household_size_Not suitable`,
      average_household_size_suitable = average_household_size_Suitable,
      average_household_size_not_suitable = `average_household_size_Not suitable`
    ) |>
    dplyr::mutate(
      percent_household_not_suitable = 
        household_size_not_suitable / 
        (household_size_suitable + household_size_not_suitable)#,
      #household_not_suitable = percent_household_not_suitable / 
      #                         max(percent_household_not_suitable, na.rm = TRUE)
    ) |>
    dplyr::select(
      DGUID, 
      percent_household_not_suitable
    ) 
    
    ## Census 2021 dwelling condition
    dc <- importdat("b48b01d6", "format")[[1]]
    dc <- dplyr::filter(dc, DGUID %in% sc2021$DGUID)
    dc <- dplyr::filter(
      dc,
      `Period of construction (13)` == "Total - Period of construction" &
      `Structural type of dwelling (10)` == "Total - Structural type of dwelling" &
      `Statistics (3C)` == "Number of private households" &
      `Dwelling condition (4)` != "Total - Dwelling condition"    
    ) |>
    dplyr::select(
      DGUID,
      dwelling_condition = `Dwelling condition (4)`,
      tenure = `Tenure (4):Total - Tenure[1]`
    ) |>
    tidyr::pivot_wider(
      id_cols = DGUID,
      names_from = dwelling_condition,
      values_from = tenure
    ) |>
    dplyr::rename(
      regular_maintenance_needed = "Regular maintenance needed",
      minor_repairs_needed = "Minor repairs are needed",
      major_repairs_needed = "Major repairs needed"
    ) |>
    dplyr::mutate(
      percent_major_repairs_needed = 
        major_repairs_needed / 
        (regular_maintenance_needed + minor_repairs_needed + major_repairs_needed)#,
      #major_repairs_needed = 
      #  percent_major_repairs_needed / max(percent_major_repairs_needed, na.rm = TRUE)
    ) |>
    dplyr::select(
      DGUID, 
      percent_major_repairs_needed
    ) 

    ## Census 2021 acceptable housing
    ah <- importdat("f4abec86", "format")[[1]]
    ah <- dplyr::filter(ah, DGUID %in% sc2021$DGUID)
    ah <- dplyr::filter(
      ah,
      `Residence on or off reserve (3)` == "Total - Residence on or off reserve" &
      `Core housing need (5)` == "Total - Core housing need" &
      `Household type including census family structure (16)` == 
        "Total - Household type including census family structure" &
      `Statistics (3C)` == "Number of private households" &
      `Acceptable housing (9)` != "Total - Acceptable housing"
    ) |>
    dplyr::select(
      DGUID,
      acceptable_housing = `Acceptable housing (9)`,
      tenure = `Tenure (4):Total - Tenure[1]`
    ) |>
    dplyr::mutate(
      acceptable_housing = tolower(stringr::str_replace(acceptable_housing," ","_"))
    ) |>
    tidyr::pivot_wider(
      id_cols = DGUID,
      names_from = acceptable_housing,
      values_from = tenure
    ) 
    iid <- stringr::str_detect(colnames(ah), "below")
    ah$below_thresholds <- rowSums(ah[, iid])
    ah$percent_below_thresholds <- ah$below_thresholds / (ah$below_thresholds + ah$acceptable)
    # ah$below_thresholds <- ah$percent_below_thresholds / 
    #                        max(ah$percent_below_thresholds, na.rm = TRUE)
    ah <- dplyr::select(
      ah,
      DGUID,
      percent_below_thresholds
    )   
    
    # Combine with census subdivisions 
    sc2021 <- dplyr::left_join(sc2021, hs, by = "DGUID") |>
              dplyr::left_join(dc, by = "DGUID") |>
              dplyr::left_join(ah, by = "DGUID")

    # Export
    fm <- here::here(path,"format",glue::glue("{nm}"))
    masterwrite(sc2021, fm)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    meta <- load_metadata(path, nm) |>
    add_format( 
      format = list(
        timestamp = timestamp(),
        description = 'Data from housing suitability [@statisticscanada2022c], dwelling condition [@statisticscanada2022d], and acceptable housing [@statisticscanada2022e] of the 2021 Census of Population [@statisticscanada2021a] were joined with the 2021 Census cartographic division boundary file [@statisticscanada2022; @statisticscanada2022f]. <br> According to Statistics Canada housing suitability *"refers to whether a private household is living in suitable accommodations according to the National Occupancy Standard (NOS); that is, whether the dwelling has enough bedrooms for the size and composition of the household. A household is deemed to be living in suitable accommodations if its dwelling has enough bedrooms, as calculated using the NOS. Housing suitability assesses the required number of bedrooms for a household based on the age, sex, and relationships among household members. An alternative variable, persons per room, considers all rooms in a private dwelling and the number of household members. Housing suitability and the National Occupancy Standard (NOS) on which it is based were developed by Canada Mortgage and Housing Corporation (CMHC) through consultations with provincial housing agencies."* Housing suitability was assessed as the proportion of households in a census division considered as not suitable. <br> Dwelling condition refers to whether the dwelling is in need of repairs. Acceptability of dwelling condition was assessed as the proportion of households in a census division considered as needing major repairs. <br> According to Statistics Canada, acceptable housing *"refers to whether a household meets each of the three indicator thresholds established by the Canada Mortgage and Housing Corporation for housing adequacy, suitability and affordability. Housing indicator thresholds are defined as follows: 1) adequate housing is reported by their residents as not requiring any major repairs; 2) affordable housing has shelter costs less than 30% of total before-tax household income;  3) suitable housing has enough bedrooms for the size and composition of resident households according to the National Occupancy Standard (NOS), conceived by the Canada Mortgage and Housing Corporation and provincial and territorial representatives. Acceptable housing identifies which thresholds the household falls below, if any. Housing that is adequate in condition, suitable in size and affordable is considered to be acceptable."* Here, acceptable housing was assessed as the proportion of households in a census division that was below any of the thresholds of adequacy, affordability or suitability.',
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
    dat <- importdat(uid, "format")[[1]] |>
           stars::st_rasterize()
    hs <- dplyr::select(dat, percent_household_not_suitable) |> masteringrid()
    dc <- dplyr::select(dat, percent_major_repairs_needed) |> masteringrid()
    ah <- dplyr::select(dat, percent_below_thresholds) |> masteringrid()
    
    # Export 
    masterwrite(hs, here::here(path, "ingrid", glue::glue("{nm}-housing_suitability")))
    masterwrite(dc, here::here(path, "ingrid", glue::glue("{nm}-dwelling_condition")))
    masterwrite(ah, here::here(path, "ingrid", glue::glue("{nm}-acceptable_housing")))
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    type <- c('housing_suitability','dwelling_condition','acceptable_housing')
    type2 <- c('Housing suitability','Dwelling condition','Acceptable housing')
    meta <- load_metadata(path, nm) |>
    add_ingrid(
      ingrid = list(
        timestamp = timestamp(),
        description = meta$format$description,
        files = list(
          filenames = glue::glue("{nm}-{type}"),
          names = glue::glue(
            "Housing acceptability in Canada - {type2}"
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
