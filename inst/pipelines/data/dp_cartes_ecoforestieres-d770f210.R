#' @eval get_name("d770f210")
#'
#' @eval get_description("d770f210")
#'
#' @eval dp_params()
#' @param feuillet code for the feuillet to downlaod
#'

#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: d770f210
#'
#' @examples
#' \dontrun{
#' dp_d770f210()
#' }
dp_d770f210 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, feuillet = NULL, ...) {
  # Output folders and other objects used
  uid <- "d770f210"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!exist$raw) {
    if (is.null(feuillet) & is.null(bbox)) {
      rlang::abort("The code of the feuillet or a bounding box must be provided in order to download the ecoforest maps")
    }

    # If the data is downloaded from online sources
    urls <- c(
      "https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Foret/DONNEES_FOR_ECO_SUD/Resultats_inventaire_et_carte_ecofor/Shp/Pee_ori.zip"
    )
    # Load
    pipeload(
      urls = urls,
      output = here::here(path, "raw"),
      large = FALSE
    )

    # Feuillets
    dat <- sf::st_read(here::here(path, "raw", "Pee_ori/Pee_ori.shp"))
    if (!is.null(feuillet)) {
      urls <- glue::glue(
        # "{dat$carte_ori[dat$feuillet == feuillet]}PRODUITS_IEQM_{feuillet}_10.zip"
        "{dat$carte_ori[dat$feuillet == feuillet]}PRODUITS_IEQM_{feuillet}_93.zip"
      )
    } else if (!is.null(bbox)) {
      bb <- bbox_poly(bbox, bbox_crs) |>
        sf::st_transform(sf::st_crs(dat))
      dat <- sf::st_intersection(dat, bb)
      feuillet <- dat$feuillet
      urls <- dat$carte_ori
      urls <- glue::glue(
        # {"{urls}PRODUITS_IEQM_{feuillet}_10.zip"}
        {
          "{urls}PRODUITS_IEQM_{feuillet}_93.zip"
        }
      )
    }
    # Load
    pipeload(
      urls = urls,
      output = here::here(path, "raw"),
      large = TRUE
    )
  }
  # _________________________________________________________________________________________ #

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # This is not reproducible
    # 31N for Val-d'Or
    dat <- sf::st_read("data/data-raw/cartes_ecoforestieres-d770f210/raw/PRODUITS_IEQM_31N_93.gdb", layer = "PEE_ORI_31N")
    bbox <- c(xmin = -78.05, ymin = 47.9, xmax = -77.5, ymax = 48.19)
    bbox_crs <- 4326
    bb <- bbox_poly(bbox, bbox_crs) |>
      sf::st_transform(sf::st_crs(dat))
    dat <- sf::st_intersection(dat, bb)

    # Classe age
    dat$age <- character(nrow(dat))
    mature <- c("12090", "120", "90", "9090", "90120", "VIN", "VIR")
    dat$age[dat$CL_AGE %in% mature] <- "Mature"

    regen <- as.character(2000:2013)
    dat$age[dat$AN_ORIGINE %in% regen] <- "Regeneration"

    cut <- as.character(2014:2022)
    dat$age[dat$AN_ORIGINE %in% cut] <- "Jeune coupe"

    # Type
    dat$type <- character(nrow(dat))
    feuillu <- "F"
    dat$type[dat$TYPE_COUV %in% feuillu] <- "Feuillus"

    conifere <- "R"
    dat$type[dat$TYPE_COUV %in% conifere] <- "Résineux"

    mixte <- "M"
    dat$type[dat$TYPE_COUV %in% mixte] <- "Mixte"
    # mapview(dat[,"type"])

    # Perturbations
    dat$perturbations <- character(nrow(dat))
    chablis <- dat$PERTURB == "CHP" | dat$ORIGINE == "CHT"
    brulis <- dat$PERTURB == "BRP" | dat$ORIGINE == "BR"
    epidemie <- dat$PERTURB == "EL" | dat$ORIGINE == "ES"
    deperis <- dat$PERTURB == "DP" | dat$ORIGINE == "DT"
    dat$perturbations[chablis] <- "Chablis"
    dat$perturbations[brulis] <- "Brulis"
    dat$perturbations[epidemie] <- "Épidémie"
    dat$perturbations[deperis] <- "Dépérissement"
    # mapview(dat[,"perturbations"])

    # Interventions
    dat$interventions <- character(nrow(dat))
    coupepart <- dat$PERTURB == "CP"
    precom <- dat$PERTURB == "EPC" | dat$PERTURB == "EPC_SYS"
    dat$interventions[coupepart] <- "Coupe partielle"
    dat$interventions[precom] <- "Coupe pré-commerciale"

    # dat$classif <- glue::glue("{dat$type}-{dat$age}-{dat$perturbations}-{dat$interventions}-{dat$TYPE_TER}")
    # dat$classif <- glue::glue("{dat$type}-{dat$TYPE_TER}")
    dat$classif <- glue::glue("{dat$type}")
    x <- dat[dat$classif != "" & !is.na(dat$classif), ]
    # mapview(dat[,'classif'], alpha = 0.5)
    mapview(x[, "classif"], alpha = 0.5)

    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # FORMAT DATA
    # WARNING: In order for filters to work, names of column should be:
    #             year      = year
    #             longitude = longitude
    #             latitude  = latitude
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

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
      data_bbox = sf::st_bbox(dat),
      data_timespan = sort(unique(dat$year))
    )

    # To add additional metadata for queried data
    meta <- add_metadata(meta,
      info1 = c("Format as lists and dataframes to be rendered as yaml"),
      info2 = c("Formatting thus matters"),
      info3 = c("Go to https://github.com/vubiostat/r-yaml for more information")
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
    # dat <- lapply(dat, dp_parameters, bbox = bbox, bbox_crs = bbox_crs, timespan = timespan)
    dat <- dp_parameters(
      dat,
      bbox = bbox,
      bbox_crs = bbox_crs,
      timespan = timespan
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    fm <- here::here(path, glue::glue("{nm}"))
    masterwrite(dat, fm)

    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
