#' @eval get_name("b39ddb9f")
#'
#' @eval get_description("b39ddb9f")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: b39ddb9f
#'
#' @examples
#' \dontrun{
#' dp_b39ddb9f()
#' }
dp_b39ddb9f <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "b39ddb9f"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  exist <- check_files(uid, name, ondisk = TRUE)
  path <- make_output(uid, name)


  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    filepath <- here::here(path, "raw", "Damed_Rivers_with_2021.dat")
    dat <- readr::read_csv(filepath)
    xy <- dat[1:78, ] |>
      apply(MARGIN = 1, FUN = stringr::str_split, pattern = "        ") |>
      lapply(function(x) setNames(x[[1]], c("longitude", "latitude"))) |>
      dplyr::bind_rows() |>
      as.data.frame() |>
      dplyr::mutate(id = 1:dplyr::n()) |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

    dat <- dat[79:nrow(dat), ] |>
      apply(MARGIN = 1, FUN = stringr::str_split, pattern = " ") |>
      lapply(function(x) {
        as.numeric(x[[1]]) |>
          na.omit() |>
          as.numeric()
      }) |>
      lapply(function(x) setNames(x, c("year", "month", glue::glue("river_{1:78}")))) |>
      dplyr::bind_rows()
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
      data_bbox = sf::st_bbox(xy),
      data_timespan = sort(unique(dat$year))
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # APPLY SUBSETS SPECIFIED BY USER
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    xy <- dp_parameters(
      xy,
      bbox = bbox,
      bbox_crs = bbox_crs
    )
    dat <- dp_parameters(
      dat,
      timespan = timespan
    )

    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    fm <- here::here(path, glue("{nm}-{c('coordinates','water_discharge')}"))
    masterwrite(xy, fm[1])
    masterwrite(dat, fm[2])

    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
