#' @eval get_name("18869625")
#'
#' @eval get_description("18869625")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param min_n_obs numeric, minimum number of observations for species to be considered
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 18869625
#'
#' @examples
#' \dontrun{
#' di_18869625()
#' }
di_18869625 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, min_n_obs = 50, ...) {
  # Output folders and other objects used
  uid <- "18869625"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

  if (!exist$integrated) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    raw_id <- get_rawid(uid) # String with data to import
    pipedat(raw_id, bbox, bbox_crs, timespan)
    dat <- importdat(raw_id)
    species <- dat[["species_list_nw_atlantic-893b37e8.csv"]] |>
               dplyr::filter(Freq >= min_n_obs)
    codes <- dat[["species_list_nw_atlantic-893b37e8-codes.csv"]]
    # _________________________________________________________________________________________ #
    
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ANALYZE / FORMAT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Observations
    obs <- dat[
      c(
        "dfo_survey_4vsw-2aafec74-gscat.csv",
        # "dfo_survey_fall-90e90110-gscat.csv",
        "dfo_survey_spring-21f8a758-gscat.csv",
        "dfo_survey_summer-3348d162-gscat.csv"
      )
    ]
    obs[["dfo_survey_4vsw-2aafec74-gscat.csv"]]$survey <- "4vsw"
    # obs[["dfo_survey_fall-90e90110-gscat.csv"]]$survey <- "fall"
    obs[["dfo_survey_spring-21f8a758-gscat.csv"]]$survey <- "spring"
    obs[["dfo_survey_summer-3348d162-gscat.csv"]]$survey <- "summer"
    obs <- dplyr::bind_rows(obs)
    
    # Add species aphiaID and filter species with >= min_n_obs
    obs <- dplyr::left_join(obs, codes[, c("aphiaID","CODE")], by = c("SPEC" = "CODE")) |>
           dplyr::filter(aphiaID %in% species$aphiaID)
    
    # Stations 
    stations <- dat[
      c(
        "dfo_survey_4vsw-2aafec74-gsinf.csv",
        # "dfo_survey_fall-90e90110-gsinf.csv",
        "dfo_survey_spring-21f8a758-gsinf.csv",
        "dfo_survey_summer-3348d162-gsinf.csv"
      )
    ]
    stations[["dfo_survey_4vsw-2aafec74-gsinf.csv"]]$STRAT <- as.character(stations[["dfo_survey_4vsw-2aafec74-gsinf.csv"]]$STRAT)
    # stations[["dfo_survey_fall-90e90110-gsinf.csv"]]$STRAT <- as.character(stations[["dfo_survey_fall-90e90110-gsinf.csv"]]$STRAT)
    stations[["dfo_survey_4vsw-2aafec74-gsinf.csv"]]$survey <- "4vsw"
    # stations[["dfo_survey_fall-90e90110-gsinf.csv"]]$survey <- "fall"
    stations[["dfo_survey_spring-21f8a758-gsinf.csv"]]$survey <- "spring"
    stations[["dfo_survey_summer-3348d162-gsinf.csv"]]$survey <- "summer"
    stations <- dplyr::bind_rows(stations)    
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "integration",
      pipeline_id = uid,
      integration_data = raw_id
    )
    # _________________________________________________________________________________________ #
    
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT 
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data   
    tmp <- c("occurrences","stations")
    fm <- here::here(path,glue::glue("{nm}-{tmp}"))
    # for(i in 1:length(dat)) masterwrite(dat[[i]], fm[i])
    masterwrite(obs, fm[1])
    masterwrite(stations, fm[2])
    
    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)  
    # _________________________________________________________________________________________ #
  }
}
