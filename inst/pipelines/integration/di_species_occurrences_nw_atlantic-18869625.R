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
        "dfo_survey_fall-90e90110-gscat.csv",
        "dfo_survey_spring-21f8a758-gscat.csv",
        "dfo_survey_summer-3348d162-gscat.csv"
      )
    ]
    obs[[1]]$survey <- "4vsw"
    obs[[2]]$survey <- "fall"
    obs[[3]]$survey <- "spring"
    obs[[4]]$survey <- "summer"
    obs <- dplyr::bind_rows(obs)
    
    # Add species aphiaID and filter species with >= min_n_obs
    obs <- dplyr::left_join(obs, codes[, c("aphiaID","CODE")], by = c("SPEC" = "CODE")) |>
           dplyr::filter(aphiaID %in% species$aphiaID)
    
    

    
    # Filter data based on minimum observation specified
    
    
    
    # gscat species catch 
    # gsinf spatial info 
    
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "integration",
      pipeline_id = uid,
      integration_data = raw_id,
      integration_grid = get_grid_info(grid) # if applicable
    )
      
    # To add additional metadata for queried data
    meta <- add_metadata(meta, 
      info1 = c("Format as lists and dataframes to be rendered as yaml"),
      info2 = c("Formatting thus matters"),
      info3 = c("Go to https://github.com/vubiostat/r-yaml for more information")
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
    fm <- here::here(path,glue::glue("{nm}"))
    # for(i in 1:length(dat)) masterwrite(dat[[i]], fm[i])
    masterwrite(dat, fm)
    
    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)  
    # _________________________________________________________________________________________ #
  }
}
