#' @eval get_name("3992e1a6")
#'
#' @eval get_description("3992e1a6")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 3992e1a6
#'
#' @examples
#' \dontrun{
#' di_3992e1a6()
#' }
di_3992e1a6 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, ...) {
  # Output folders and other objects used
  uid <- "3992e1a6"
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

    # Years     
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
    iid <- datnames$months %in% c("05","06","07","08","09","10","11")
    datnames <- datnames[iid, ]
    dat <- dat[iid]


    # Study grid, if applicable
    if (is.null(grid)) {
      grid <- stars::read_stars("data/data-grid/grid_raster.tif", quiet = TRUE)
    }
    if (sf::st_crs(grid)$epsg != 4326) {
      grid <- sf::st_transform(grid, crs = 4326)
    }
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
    
    # Positive & negative anomalies
    pos <- neg <- sst
    for (i in 1:length(sst)) {
      pos[[i]]$anomalies <- ifelse(pos[[i]]$anomalies < 0, NA, pos[[i]]$anomalies)
      neg[[i]]$anomalies <- ifelse(neg[[i]]$anomalies > 0, NA, neg[[i]]$anomalies)
    }

    # Transform negative anomalies as positive values
    for (i in 1:length(sst)) {
      neg[[i]]$anomalies <- abs(neg[[i]]$anomalies)
    }
    
    # Annual sum
    # NOTE: this part should be removed and the rest of the code adjusted accordingly 
    #       for monthly rather than annual rasters
    annualSum <- function(x, iid) {
      x <- x[iid] |>
           lapply(dplyr::select, anomalies) |>
           dplyr::bind_cols() |>
           rowSums(na.rm = TRUE)
      cbind(pos[[1]][,c("longitude","latitude")], x)
    }
    pos_ann <- neg_ann <- list()
    for(i in 1:length(ys)) {
      iid <- which(datnames$years == ys[i])
      pos_ann[[i]] <- annualSum(pos, iid)
      neg_ann[[i]] <- annualSum(neg, iid)
      colnames(pos_ann[[i]])[3] <- glue::glue("{nm}-positive-{ys[i]}")
      colnames(neg_ann[[i]])[3] <- glue::glue("{nm}-negative-{ys[i]}")
    } 
    pos <- pos_ann 
    neg <- neg_ann

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                                 RASTERIZE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Spatial objects
    spat <- function(x) {
      sf::st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326) |>
      sf::st_transform(crs = 32198)
    }
    pos <- lapply(pos, spat)
    neg <- lapply(neg, spat)
    
    # Rasterize and warp to study area grid
    pipedat_rasterize <- function(pts) {
      dat <- stars::st_rasterize(pts, dx = 2000, dy = 2000) |>
        stars::st_warp(grid) |>
        c(grid) |>
        as.data.frame() |>
        dplyr::filter(!is.na(grid_raster.tif)) |>
        dplyr::arrange(uid) |>
        dplyr::select(-x, -y) |>
        stats::setNames(c("intensity", "uid")) |>
        dplyr::filter(intensity > 0) |>
        dplyr::select(uid, intensity)
    }
    pos <- lapply(pos, pipedat_rasterize)
    neg <- lapply(neg, pipedat_rasterize)
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
    fm_pos <- here::here(path,glue::glue("{nm}-positive-{ys}"))
    # fm_pos <- here::here(path,glue::glue("{nm}-positive-{datnames$years}-{datnames$months}"))
    fm_neg <- here::here(path,glue::glue("{nm}-negative-{ys}"))
    # fm_neg <- here::here(path,glue::glue("{nm}-negative-{datnames$years}-{datnames$months}"))
    for(i in 1:length(pos)) masterwrite(pos[[i]], fm_pos[i])
    for(i in 1:length(neg)) masterwrite(neg[[i]], fm_neg[i])
    
    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)  
    # _________________________________________________________________________________________ #
  }
}
