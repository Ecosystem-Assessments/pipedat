#' @eval get_name("8449dee0")
#'
#' @eval get_description("8449dee0")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 8449dee0
#'
#' @examples
#' \dontrun{
#' dp_8449dee0()
#' }
dp_8449dee0 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "8449dee0"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  exist <- check_files(uid, name, ondisk = TRUE)
  path <- make_output(uid, name)

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT & FORMAT DATA
    # NOTE: optional
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Unzip
    zipfiles <- dir(glue("{path}/raw"), full.names = TRUE)
    lapply(zipfiles, utils::unzip, exdir = glue("{path}/raw"))

    # -----
    # NOTE: Very large datasets, hence we crop the dataset right away
    #       rather than wait later in the script.
    # Manual settings, to change if the pipeline and data are updated
    if (is.null(timespan)) {
      years <- 2017:2020
    } else {
      years <- timespan
    }

    # Data bounding box (to measure as we load files since we subset them right away)
    update_bb <- function(bb, xmin, ymin, xmax, ymax) {
      bb["xmin"] <- ifelse(xmin < bb["xmin"], xmin, bb["xmin"])
      bb["ymin"] <- ifelse(ymin < bb["ymin"], ymin, bb["ymin"])
      bb["xmax"] <- ifelse(xmax > bb["xmax"], xmax, bb["xmax"])
      bb["ymax"] <- ifelse(ymax > bb["ymax"], ymax, bb["ymax"])
      bb
    }

    # Interpolated data
    inter <- dir(
      glue("{path}/raw/shipping_Dec_2021"),
      pattern = "_interpolated.csv",
      full.names = TRUE
    )

    # Non interpolated data
    nointer <- dir(
      glue("{path}/raw/shipping_Dec_2021"),
      pattern = "_noninterpolated.csv",
      full.names = TRUE
    )

    # -----
    # WARNING: Do not reuse this bb object elsewhere as a starting point
    #          Technically this could provide a wrong bounding box.
    #          I use this only as a starting point because I know it's a global dataset
    bb <- c(xmin = 0, ymin = 0, xmax = 0, ymax = 0)
    lon_bin <- lat_bin <- NULL # for R CMD CHECK
    dat <- ndat <- list()
    for (i in 1:length(inter)) {
      # Import file
      dat[[i]] <- utils::read.csv(inter[i]) |>
        dplyr::rename(longitude = lon_bin, latitude = lat_bin)

      ndat[[i]] <- utils::read.csv(nointer[i]) |>
        dplyr::rename(longitude = lon_bin, latitude = lat_bin)

      # Update bounding box
      bb <- update_bb(
        bb,
        xmin = min(dat[[i]]$longitude),
        ymin = min(dat[[i]]$latitude),
        xmax = max(dat[[i]]$longitude),
        ymax = max(dat[[i]]$latitude)
      )

      # Crop
      dat[[i]] <- dp_parameters(dat[[i]], bbox = bbox, bbox_crs = bbox_crs, timespan = years)
      ndat[[i]] <- dp_parameters(ndat[[i]], bbox = bbox, bbox_crs = bbox_crs, timespan = years)
    }

    dat <- dplyr::bind_rows(dat)
    ndat <- dplyr::bind_rows(ndat)

    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox,
      pipeline_bbox_crs = bbox_crs,
      pipeline_timespan = timespan,
      access = "2022-02-28",
      data_bbox = bb,
      data_timespan = 2017:2020
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
    fm <- glue("{path}/{nm}")
    utils::write.csv(dat, glue("{fm}-interpolated.csv"), row.names = FALSE)
    utils::write.csv(ndat, glue("{fm}-noninterpolated.csv"), row.names = FALSE)
    unlink(glue("{path}raw/shipping_Dec_2021/"), recursive = TRUE)

    # Metadata
    mt <- glue("{path}/{nm}.yaml")
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- glue("{path}/{nm}.bib")
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
