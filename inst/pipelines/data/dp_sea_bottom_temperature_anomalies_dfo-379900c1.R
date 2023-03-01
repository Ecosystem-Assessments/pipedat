#' @eval get_name("379900c1")
#'
#' @eval get_description("379900c1")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 379900c1
#'
#' @examples
#' \dontrun{
#' dp_379900c1()
#' }
dp_379900c1 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "379900c1"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid, ondisk = TRUE)
  path <- make_output(uid)

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    utils::unzip(
      here::here(path, "raw", "SBT.zip"),
      exdir = here::here(path, "raw", "SBT")
    )
    
    # Data grid 
    x <- read.table(here::here(path,"raw","x.grid"))
    y <- read.table(here::here(path,"raw","y.grid"))
    xy <- expand.grid(y$V1, x$V1) |>
          dplyr::rename(latitude = Var1, longitude = Var2) |>
          dplyr::select(longitude, latitude)

    # St. Lawrence Grid 
    xR <- range(-x[,1])
    yR <- range(y[,1])
    aoi <- c(xmin =xR[1], ymin = yR[1], xmax = xR[2], ymax = yR[2]) |>
           sf::st_bbox(crs = 4326) |>
           sf::st_as_sfc() |>
           sf::st_as_sf() |>
           stars::st_rasterize(dx = 0.01, dy = 0.01)

    # Data    
    years <- dir(here::here(path, "raw", "SBT"))
    if (!is.null(timespan)) years <- years[as.numeric(years) %in% timespan]
    files <- dir(here::here(path, "raw", "SBT", years), recursive = TRUE, full.names = TRUE)
    dat <- list()
    for(i in 1:length(files)) {
      # Matrix as numeric 
      x <- read.table(files[i]) |>
             as.matrix() |>
             as.vector()
      
      # Join to coordinates and set NA values
      x <- cbind(xy, x) |>
           dplyr::mutate(longitude = -longitude) |>
           dplyr::mutate(x = ifelse(x == -9, NA, x))
      colnames(x)[3] <- glue::glue("sea_bottom_temperature_anomalies_dfo-379900c1-{years[i]}")
            
      # Create stars object
      x <- stars::st_as_stars(x, coords = c("longitude","latitude"))
      sf::st_crs(x) <- sf::st_crs(4326)
      x <- stars::st_warp(x, aoi)
      
      dat[[i]] <- x
    }
    # _________________________________________________________________________________________ #


    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox,
      pipeline_bbox_crs = bbox_crs,
      pipeline_timespan = timespan,
      access = timestamp(),
      data_bbox = sf::st_bbox(dat[[1]]),
      data_timespan = 2010:2022
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT METADATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Data
    fm <- here::here(path, glue::glue("{nm}-{years}"))
    for (i in 1:length(dat)) masterwrite(dat[[i]], fm[i])

    # Remove unzipped data to avoid using memory unnecessarily
    unlink(here::here(path, "raw", "SBT"), recursive = TRUE)

    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
