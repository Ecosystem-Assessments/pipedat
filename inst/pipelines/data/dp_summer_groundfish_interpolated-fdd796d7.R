#' @eval get_name("fdd796d7")
#'
#' @eval get_description("fdd796d7")
#'
#' @eval dp_params()
#' @param groundfish_variables name of variables to import from the summer groundfish interpolated
#' results Possible entries are: "sea_water_temperature","sea_water_temperature_anomaly",
#' "sea_water_practical_salinity","sea_water_practical_salinity_anomaly",
#' "sea_floor_depth_below_sea_surface"
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: fdd796d7
#'
#' @examples
#' \dontrun{
#' dp_fdd796d7()
#' }
dp_fdd796d7 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, groundfish_variables = c("sea_water_temperature", "sea_water_temperature_anomaly", "sea_water_practical_salinity", "sea_water_practical_salinity_anomaly"), ...) {
  # Output folders and other objects used
  uid <- "fdd796d7"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!exist$raw) {
    # List of files on ftp server
    # From: https://gist.github.com/adamhsparks/18f7702906f33dd66788e0078979ff9a
    ftp_base <- "ftp://198.103.183.98/GreenanB/summerGroundfishInterpolatedResults/"
    list_files <- curl::new_handle()
    curl::handle_setopt(list_files, ftp_use_epsv = TRUE, dirlistonly = TRUE)
    con <- curl::curl(url = ftp_base, "r", handle = list_files)
    files <- readLines(con)
    close(con)

    # Load
    urls <- glue::glue("{ftp_base}{files}")
    # Maybe a connection problem causing this
    # Unsure whether it's on my end or if the ftp is limiting the downloads
    for (i in 1:length(urls)) {
      pipeload(
        urls = urls[i],
        output = here::here(path, "raw"),
        large = TRUE
      )
    }
  }
  # _________________________________________________________________________________________ #

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    files <- dir(here::here(path, "raw"), full.names = TRUE)
    years <- substr(files, nchar(files) - 6, nchar(files) - 3) |> as.numeric()
    if (!is.null(timespan)) files <- files[years %in% timespan]
    dat <- lapply(
      files,
      stars::read_ncdf,
      var = groundfish_variables
    )
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
      data_timespan = 1970:2020
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
    # on.exit(sf::sf_use_s2(TRUE), add = TRUE)
    # sf::sf_use_s2(FALSE)
    dat <- lapply(dat, dp_parameters, bbox = bbox, bbox_crs = bbox_crs)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    name <- tools::file_path_sans_ext(basename(files))
    name <- substr(name, 48, nchar(name))
    fm <- here::here(path, glue::glue("{nm}-{name}"))
    for (i in 1:length(dat)) masterwrite(dat[[i]], fm[i])

    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
