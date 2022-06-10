#' @eval get_name("d87b7f5f")
#'
#' @eval get_description("d87b7f5f")
#'
#' @eval dp_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: d87b7f5f
#'
#' @examples
#' \dontrun{
#' dp_d87b7f5f()
#' }
dp_d87b7f5f <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "d87b7f5f"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  exist <- check_files(uid, name, ondisk = TRUE)
  path <- make_output(uid, name)

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # NOTE: optional
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Unzip files
    pathsst <- here::here(path, "raw", "sst")
    utils::unzip(
      here::here(path, "raw", "SSTanomaly2013-2017.zip"),
      exdir = pathsst
    )

    # Get folder names (years and months)
    ys <- dir(pathsst) |>
      as.numeric()

    ms <- dir(here::here(pathsst, ys[1]))

    # Create array to store monthly data per year
    sst <- vector("list", 12)

    # Replicate for the number of years
    sst <- replicate(length(ys), sst, simplify = FALSE)

    # Load files
    for (i in 1:length(ys)) {
      for (j in 1:length(ms)) {
        filePath <- dir(
          here::here(pathsst, ys[i], ms[j]),
          pattern = "anomaly.dat",
          full.names = TRUE
        )
        sst[[i]][[j]] <- utils::read.table(filePath)
      }
    }

    # Delete data, keep only zip file
    unlink(here::here(path, "raw", "sst"), recursive = TRUE)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # FORMAT DATA
    # NOTE: optional
    # WARNING: In order for filters to work, names of column should be:
    #             years     = years
    #             longitude = longitude
    #             latitude  = latitude
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ----------------------------------------------------- #
    #                     CHECK OUTLIERS                    #
    # ----------------------------------------------------- #
    # # Visualize outliers
    # par(mfrow = c(3,2))
    # for(i in 1:5) {
    #   graphicsutils::plot0(xlim = c(0,12), ylim = c(-20, 20))
    #   mtext(ys[i], 3, font = 2)
    #   for(j in 1:12) {
    #     boxplot(sst[[i]][[j]]$V3, add = T, at = j, frame = F, range = 3)
    #   }
    #   axis(1, at = 1:12, labels = 1:12)
    # }

    # Identify and modify outliers in the dataset
    for (i in 1:length(ys)) {
      for (j in 1:length(ms)) {
        out <- grDevices::boxplot.stats(sst[[i]][[j]]$V3, coef = 3)$stat[c(1, 5)]
        cap <- stats::quantile(sst[[i]][[j]]$V3, probs = c(.05, .95), na.rm = T)
        sst[[i]][[j]]$V3[sst[[i]][[j]]$V3 < out[1]] <- cap[1]
        sst[[i]][[j]]$V3[sst[[i]][[j]]$V3 > out[2]] <- cap[2]
      }
    }

    # # Visualize again
    # par(mfrow = c(3,2))
    # for(i in 1:5) {
    #   graphicsutils::plot0(xlim = c(0,12), ylim = c(-20, 20))
    #   mtext(ys[i], 3, font = 2)
    #   for(j in 1:12) {
    #     boxplot(sst[[i]][[j]]$V3, add = T, at = j, frame = F, range = 3)
    #   }
    #   axis(1, at = 1:12, labels = 1:12)
    # }

    # ----------------------------------------------------- #
    #                    SPATIAL OBJECTS                    #
    # ----------------------------------------------------- #
    # Data.frame with all unique coordinates
    coords <- data.frame(V1 = numeric(), V2 = numeric())
    for (i in 1:2) {
      for (j in 1:length(ms)) {
        coords <- rbind(coords, sst[[i]][[j]][, 1:2])
      }
    }
    coords <- unique(coords)

    # Join datasets per year
    sst2 <- vector("list", length(ys))
    for (i in 1:length(ys)) {
      sst2[[i]] <- coords
      for (j in 1:12) {
        sst2[[i]] <- dplyr::left_join(sst2[[i]], sst[[i]][[j]],
          by = c("V1", "V2")
        )
      }
      # Change column names
      colnames(sst2[[i]]) <- c("latitude", "longitude", ms)
    }

    # Replace and remove objects to save memory
    sst <- sst2
    rm(sst2)

    # Select only months from May to November due to biases in anomalies measurements
    for (i in 1:length(ys)) {
      sst[[i]] <- sst[[i]][, -c(3, 4, 5, 6, 14)]
    }

    # Add years
    for (i in 1:length(ys)) {
      sst[[i]]$year <- ys[i]
    }

    # Single dataset
    dat <- dplyr::bind_rows(sst)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    datbbox <- c(
      xmin = min(dat$longitude, na.rm = TRUE),
      ymin = min(dat$latitude, na.rm = TRUE),
      xmax = max(dat$longitude, na.rm = TRUE),
      ymax = max(dat$latitude, na.rm = TRUE)
    )
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox,
      pipeline_bbox_crs = bbox_crs,
      pipeline_timespan = timespan,
      access = timestamp(),
      data_bbox = datbbox,
      data_timespan = sort(unique(dat$year))
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # APPLY SUBSETS AND CRS SPECIFIED BY USER
    # NOTE: optional, only if applicable
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    dat <- dp_parameters(
      dat,
      bbox = bbox,
      bbox_crs = bbox_crs,
      data_crs = 4326,
      timespan = timespan
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    fm <- here::here(path, glue("{nm}.csv"))
    utils::write.csv(dat, fm, row.names = FALSE)

    # Metadata
    mt <- here::here(path, glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
