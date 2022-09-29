#' @eval get_name("608c7f2f")
#'
#' @eval get_description("608c7f2f")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 608c7f2f
#'
#' @examples
#' \dontrun{
#' di_608c7f2f()
#' }
di_608c7f2f <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, ...) {
  # Output folders and other objects used
  uid <- "608c7f2f"
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

    if (is.null(grid)) {
      grid <- stars::read_stars("data/data-grid/grid_raster.tif", quiet = TRUE)
    }
    grid <- sf::st_transform(grid, sf::st_crs(dat[[1]]))
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ANALYZE / FORMAT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    name <- tools::file_path_sans_ext(names(dat))
    for (i in 1:length(dat)) {
      dat[[i]] <- masteringrid(
        dat[[i]],
        grid = NULL,
        name = name[i]
      )
    }
    for (i in length(dat):1) {
      if (nrow(dat[[i]]) == 0) dat[[i]] <- NULL
    }
    name <- tools::file_path_sans_ext(names(dat))

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
    name <- gsub("0d61380a", "608c7f2f", name)
    fm <- here::here(path, glue::glue("{name}.csv"))
    for (i in 1:length(dat)) utils::write.csv(dat[[i]], fm[i], row.names = FALSE)

    # Metadata
    mt <- here::here(path, glue::glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue::glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  }
}
