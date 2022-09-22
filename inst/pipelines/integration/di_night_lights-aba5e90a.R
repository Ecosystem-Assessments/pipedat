#' @eval get_name("aba5e90a")
#'
#' @eval get_description("aba5e90a")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: aba5e90a
#'
#' @examples
#' \dontrun{
#' di_aba5e90a()
#' }
di_aba5e90a <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, ...) {
  # Output folders and other objects used
  uid <- "aba5e90a"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  exist <- check_files(uid, name, ondisk = FALSE)
  path <- make_output(uid, name)

  if (!exist$integrated) {
    # WARNING: For R CMD CHECK
    grid_raster.tif <- x <- y <- NULL
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    raw_id <- get_rawid(uid)
    pipedat(raw_id, bbox, bbox_crs, timespan)
    dat <- importdat(raw_id)

    if (is.null(grid)) {
      grid <- stars::read_stars("data/data-grid/grid_raster.tif", quiet = TRUE)
    }
    if (sf::st_crs(grid)$epsg != sf::st_crs(dat[[1]])$epsg) {
      grid <- sf::st_transform(grid, crs = st_crs(dat[[1]]))
    }
    names(grid) <- "uid"
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ANALYZE / FORMAT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    years <- names(dat)
    years <- substr(years, start = nchar(years) - 7, stop = nchar(years) - 4)
    nightlights <- list()
    for (i in 1:length(dat)) {
      nightlights[[i]] <-
        stars::st_warp(dat[[i]], grid) |>
        c(grid) |>
        as.data.frame() |>
        dplyr::arrange(uid) |>
        dplyr::select(-x, -y)
      colnames(nightlights[[i]]) <- c("nightlights", "uid")
      nightlights[[i]] <- dplyr::filter(nightlights[[i]], !is.na(nightlights)) |>
        dplyr::filter(!is.na(uid)) |>
        dplyr::filter(nightlights > 0) |>
        dplyr::select(uid, nightlights)
    }
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "integration",
      pipeline_id = uid,
      integration_data = raw_id,
      integration_grid = get_grid_info(grid)
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
    fm <- here::here(path, glue("{nm}-{years}.csv"))
    for (i in 1:length(years)) utils::write.csv(nightlights[[i]], fm[i], row.names = FALSE)

    # Metadata
    mt <- here::here(path, glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  }
}
