#' @eval get_name("5c829738")
#'
#' @eval get_description("5c829738")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 5c829738
#'
#' @examples
#' \dontrun{
#' di_5c829738()
#' }
di_5c829738 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, ...) {
  # Output folders and other objects used
  uid <- "5c829738"
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

    # Years & months
    datnames <- names(dat)
    ysms <- substr(datnames, nchar(datnames)-9, nchar(datnames)-4)
    ys <- substr(ysms, 1,4)
    ms <- substr(ysms, 5,6)
    datnames <- data.frame(
      names = names(dat),
      years = ys,
      months = ms
    )
    ys <- unique(ys)
    ms <- unique(ms)

    # Select only months from May to November due to biases in anomalies measurements
    # iid <- datnames$months %in% c("05","06","07","08","09","10","11")
    # datnames <- datnames[iid, ]
    # dat <- dat[iid]
    # _________________________________________________________________________________________ #
    
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ANALYZE / FORMAT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ----------------------------------------------------- #
    #                     CHECK OUTLIERS                    #
    # ----------------------------------------------------- #
    # Identify and modify outliers in the dataset
    for (i in 1:length(dat)) {
      out <- grDevices::boxplot.stats(dat[[i]]$anomalies, coef = 3)$stat[c(1, 5)]
      cap <- stats::quantile(dat[[i]]$anomalies, probs = c(.05, .95), na.rm = TRUE)
      dat[[i]]$anomalies[dat[[i]]$anomalies < out[1]] <- cap[1]
      dat[[i]]$anomalies[dat[[i]]$anomalies > out[2]] <- cap[2]
    }
    
    # ----------------------------------------------------- #
    #                    SPATIAL OBJECTS                    #
    # ----------------------------------------------------- #
    # Data.frame with all unique coordinates
    coords <- dat[[1]][,c("longitude","latitude")]
    for(i in 2:length(dat)) {
      coords <- rbind(coords, dat[[i]][,c("longitude","latitude")]) |>
                unique()
    }
    coords <- dplyr::arrange(coords, longitude, latitude)
    sst <- list()
    for(i in 1:length(dat)) {
      sst[[i]] <- dplyr::left_join(
        coords, 
        dat[[i]][,c("longitude","latitude","anomalies")], 
        by = c("longitude","latitude"))
    }

    # Transform values between -.5 and +.5 to NA
    trans <- function(x) {
      x$anomalies <- ifelse(x$anomalies > -.5 & x$anomalies < .5, NA, x$anomalies)
      x
    }
    sst <- lapply(sst, trans)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                                 RASTERIZE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Spatial objects
    spat <- function(x) {
      sf::st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326) |>
      sf::st_transform(crs = 32198)
    }
    sst <- lapply(sst, spat)
    
    # Rasterize and warp to study area grid
    pipedat_rasterize <- function(pts) {
      dat <- stars::st_rasterize(pts, dx = 2000, dy = 2000) |>
        masteringrid() 
    }
    sst <- lapply(sst, pipedat_rasterize)
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
    dates <- expand.grid(ys, ms) |> dplyr::arrange(Var1, Var2)
    fm <- here::here(path,glue::glue("{nm}-positive-{dates$Var1}_{dates$Var2}"))
    for(i in 1:length(sst)) masterwrite(sst[[i]], fm[i])
    
    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)  
    # _________________________________________________________________________________________ #
  }
}
