#' @eval get_name("6dba9a9f")
#'
#' @eval get_description("6dba9a9f")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 6dba9a9f
#'
#' @examples
#' \dontrun{
#' di_6dba9a9f()
#' }
di_6dba9a9f <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, ...) {
  # Output folders and other objects used
  uid <- "6dba9a9f"
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

    # Select required data only : bottom values
    iid <- which(stringr::str_detect(names(dat), "bottomValues"))
    dat <- dat[iid]
    
    # Name for export 
    name <- tools::file_path_sans_ext(names(dat)) |>
      substr(40, 57) |>
      stringr::str_replace("_","-")
    name <- glue::glue("{nm}{name}")
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ANALYZE / FORMAT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ------------------------------------
    # # Smoothing objects & parameters
    # resolution <- 1000
    # bandwidth <- 50000
    # grd <- sf::st_bbox(dat[[1]], crs = 4326) |>
    #        sf::st_as_sfc() |>
    #        sf::st_as_sf() |> 
    #        stars::st_rasterize(dx = 0.1, dy = 0.1) 
    # grd <- as(grd, "Raster")
    # 
    # # Smoothing function for predictions
    # smooth_predict <- function(dat, resolution, bandwidth, grd) {
    #   # Prepare data 
    #   ## Technically, a sf object could be provided to the btb_smooth function, but there 
    #   ## is a problem with the epsg checks when its length it greater than 4 characters. 
    #   ## See Issue #5 https://github.com/InseeFr/btb/issues/5
    #   dat <- data.frame(dat) |> na.omit() |>
    #          sf::st_as_sf(coords = c("x","y"), crs = 4326) |>
    #          sf::st_transform(crs = 32198) 
    #   dat <- cbind(sf::st_coordinates(dat), dat) |>
    #          sf::st_drop_geometry() |>
    #          dplyr::rename(x = X, y = Y)
    # 
    #   # Smoothing
    #   suppressMessages({
    #     kernel <- btb::btb_smooth(
    #                 pts = dat,
    #                 sEPSG = "32198",
    #                 iCellSize = resolution,
    #                 iBandwidth = bandwidth
    #               )
    #   })    
    #   kernel[,3] <- ifelse(kernel[,3,drop=T] > 1, 1, kernel[,3,drop=T])
    #   kernel <- sf::st_transform(kernel, crs = 4326)
    # 
    #   # Rasterize and return
    #   fasterize::fasterize(
    #     sf = kernel,
    #     raster = grd, 
    #     field = colnames(kernel)[3], 
    #     fun = "max"
    #   ) 
    # }
    
    # ------------------------------------
    pos <- neg <- list()
    for(i in 1:length(dat)) {
      names(dat[[i]]) <- "sbt"
      
      # # Kernel smoothing
      # y <- smooth_predict(
      #   dat = x, 
      #   resolution = resolution, 
      #   bandwidth = bandwidth, 
      #   grd = grd
      # )
      
      # Positive and negative, and only select values > 0.5 or < -0.5
      pos[[i]] <- dplyr::mutate(dat[[i]], sbt = ifelse(sbt < 0.5, NA, sbt))
      neg[[i]] <- dplyr::mutate(dat[[i]], sbt = ifelse(sbt > -0.5, NA, -sbt))
      
      # In grid
      pos[[i]] <- masteringrid(pos[[i]])
      neg[[i]] <- masteringrid(neg[[i]])
    }
    names(pos) <- names(neg) <- name
    names(pos) <- gsub("bottomValues","positive", names(pos))
    names(neg) <- gsub("bottomValues","negative", names(neg))
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
    fm1 <- here::here(path, names(pos))
    fm2 <- here::here(path, names(neg))
    for (i in 1:length(pos)) masterwrite(pos[[i]], fm1[i])
    for (i in 1:length(neg)) masterwrite(neg[[i]], fm2[i])

    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)
    # _________________________________________________________________________________________ #
  }
}
