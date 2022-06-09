#' @eval get_name("f2109e69")
#'
#' @eval get_description("f2109e69")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: f2109e69
#'
#' @examples
#' \dontrun{
#' dp_f2109e69()
#' }
dp_f2109e69 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "f2109e69"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  exist <- check_files(uid, name, ondisk = TRUE)
  path <- make_output(uid, name)

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # NOTE: optional
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    latit_GIS <- longit_GIS <- Remarques <- NULL # for R CMD CHECK
    # Function to import ZIFF data
    import_ziff <- function(filename) {
      utils::read.csv(glue("{path}/raw/{filename}")) |>
        dplyr::filter(!is.na(latit_GIS) & !is.na(longit_GIS))
    }

    d <- list()
    d[[1]] <- import_ziff("Version_totale_20002004.csv")
    d[[2]] <- import_ziff("Version_totale_20052009.csv")
    d[[3]] <- import_ziff("Version_totale_20102014.csv")
    d[[4]] <- import_ziff("Version_totale_20152019.csv")
    d[[5]] <- import_ziff("Version_totale_20202024.csv")
    dat <- dplyr::bind_rows(d)

    # Gear types
    gear <- utils::read.csv(glue("{path}/raw/Codes_engin.csv"))

    # Species list
    species <- utils::read.csv(glue("{path}/raw/Codes_especes.csv")) |>
      dplyr::select(-Remarques)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # FORMAT DATA
    # NOTE: optional
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Adjust dates
    iid <- is.na(dat$date_cap)
    dat$date_cap[iid] <- dat$date_deb[iid]
    dat$year <- format(as.Date(dat$date_cap), format = "%Y")
    dat <- dplyr::rename(dat, longitude = longit_GIS, latitude = latit_GIS)
    dat_bbox <- c(
      xmin = min(dat$longitude, na.rm = TRUE),
      ymin = min(dat$latitude, na.rm = TRUE),
      xmax = max(dat$longitude, na.rm = TRUE),
      ymax = max(dat$latitude, na.rm = TRUE)
    )
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
      access = "2021-06-11",
      data_bbox = dat_bbox,
      data_timespan = sort(unique(dat$year)),
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
    # Require bbox in epsg:4326
    bb <- bbox_poly(bbox, bbox_crs) |>
      sf::st_transform(4326) |>
      sf::st_bbox()
    dat <- dp_parameters(
      dat,
      bbox = bb,
      bbox_crs  = bbox_crs,
      timespan = timespan
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Fisheries data
    fm <- here::here(path, glue("{nm}.csv"))
    utils::write.csv(dat, fm, row.names = FALSE)

    # Gear type
    gr <- glue("{path}/{nm}_gear.csv")
    utils::write.csv(gear, gr, row.names = FALSE)

    # Species list
    sp <- glue("{path}/{nm}_species.csv")
    utils::write.csv(species, sp, row.names = FALSE)

    # Metadata
    mt <- glue("{path}/{nm}.yaml")
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- glue("{path}/{nm}.bib")
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
