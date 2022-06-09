#' @eval get_name("3d1bfb8e")
#'
#' @eval get_description("3d1bfb8e")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 3d1bfb8e
#'
#' @examples
#' \dontrun{
#' dp_3d1bfb8e()
#' }
dp_3d1bfb8e <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "3d1bfb8e"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  exist <- check_files(uid, name, ondisk = TRUE)
  path <- make_output(uid, name)


  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # NOTE: optional
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Example for data that needs to be locally available
    utils::unzip(
      here::here(path, "raw", "CetaceanHeatmapRasters.zip"),
      exdir = here::here(path, "raw")
    )
    files <- dir(
      here::here(path, "raw", "CetaceanHeatmapRasters"),
      pattern = ".tif$",
      full.names = TRUE
    )

    # Species names
    name <- data.frame(
      nm = c(
        "beluga", "blue_whale", "northern_bottlenose_whale",
        "fin_whale", "humpback_whale", "leatherback_turtle",
        "minke_whale", "north_atlantic_right_whale", "sei_whale",
        "sperm_whale"
      ),
      fr = c(
        "Beluga du Saint-Laurent", "Rorqual bleu",
        "Baleine a bec commune", "Rorqual commun",
        "Rorqual a bosse", "Tortue luth", "Petit rorqual",
        "Baleine noire de l atlantique Nord",
        "Rorqual boreal", "Cachalot macrocephale"
      ),
      en = c(
        "St. Lawrence beluga whale", "Blue whale",
        "Northern bottlenose whale", "Fin whale",
        "Humpback whale", "Leatherback turtle", "Minke whale",
        "North Atlantic right whale", "Sei whale",
        "Sperm whale"
      )
    )

    # ID of log-transformed data to load
    iid <- 4:13
    dat <- list()
    for (i in 1:length(iid)) {
      dat[[i]] <- stars::read_stars(
        files[iid[i]],
        quiet = TRUE,
        proxy = TRUE
      )
    }
    names(dat) <- name$nm
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox,
      access = timestamp(),
      data_bbox = sf::st_bbox(dat[[1]]),
      data_timespan = 2015:2020
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
    for (i in 1:length(dat)) {
      dat[[i]] <- dp_parameters(
        dat[[i]],
        crs = 4326,
        bbox = bbox
      )
    }
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    fm <- here::here(path, glue("{nm}-{name$nm}.tif"))
    for (i in 1:length(fm)) stars::write_stars(dat[[i]], fm[i])

    # Metadata
    mt <- here::here(path, glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
