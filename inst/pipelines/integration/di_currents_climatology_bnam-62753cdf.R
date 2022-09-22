#' @eval get_name("62753cdf")
#'
#' @eval get_description("62753cdf")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 62753cdf
#'
#' @examples
#' \dontrun{
#' di_62753cdf()
#' }
di_62753cdf <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, ...) {
  # Output folders and other objects used
  uid <- "62753cdf"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  exist <- check_files(uid, name, ondisk = FALSE)
  path <- make_output(uid, name)

  if (!exist$integrated) {
    # WARNING: For R CMD CHECK
    x <- y <- NULL
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    raw_id <- get_rawid(uid) # String with data to import
    pipedat(raw_id, bbox, bbox_crs, timespan)
    dat <- importdat(raw_id)

    # Study grid, if applicable
    if (is.null(grid)) {
      grid <- stars::read_stars("data/data-grid/grid_raster.tif", quiet = TRUE)
    }
    if (sf::st_crs(grid)$epsg != 4326) {
      grid <- sf::st_transform(grid, crs = 4326)
    }
    names(grid) <- "uid"
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ANALYZE / FORMAT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    pipedat_rasterize <- function(dat) {
      pos <- sf::st_as_sf(
        dat,
        coords = c("longitude", "latitude"),
        crs = 4326
      )
      ras <- stars::st_rasterize(pos, dx = 1 / 12, dy = 1 / 12) |>
        stars::st_warp(grid) |>
        c(grid) |>
        as.data.frame() |>
        dplyr::filter(!is.na(uid)) |>
        dplyr::arrange(uid) |>
        dplyr::select(-x, -y)
      dat <- list()
      descr <- c("U", "V", "Direction", "Magnitude")
      for (i in 1:4) {
        dat[[i]] <- ras[, c("uid", descr[i])]
      }
      invisible(dat)
    }
    dat <- lapply(dat, pipedat_rasterize)
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
    descr <- c("U", "V", "Direction", "Magnitude")
    name <- tools::file_path_sans_ext(names(dat))
    name <- gsub("906f1155", "62753cdf", name)
    fm <- here::here(path, name)
    for (i in 1:length(dat)) {
      fm2 <- glue("{fm[i]}-{tolower(descr)}.csv")
      for (j in 1:length(dat[[i]])) {
        utils::write.csv(dat[[i]][[j]], fm2[j], row.names = FALSE)
      }
    }

    fm <- here::here(path, glue("{nm}.csv"))
    utils::write.csv(dat, fm, row.names = FALSE)

    # Metadata
    mt <- here::here(path, glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  }
}
