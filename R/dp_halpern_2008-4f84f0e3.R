#' @eval get_name("4f84f0e3")
#'
#' @eval get_description("4f84f0e3")
#'
#' @eval dp_params()
#' @param halpern_layers name of layers to download from the Halpern dataset. Possible entries are: "artisanal_fishing","demersal_destructive_fishing","demersal_destructive_fishing","demersal_nondest_high_bycatch","demersal_nondest_low_bycatch","inorganic","invasives","night_lights","ocean_acidification","ocean_pollution","oil_rigs_lzw","pelagic_high_bycatch","pelagic_low_bycatch","plumes_fert","plumes_pest","population","shipping","sst","uv".
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 4f84f0e3
#'
#' @examples
#' \dontrun{
#' dp_4f84f0e3()
#' }
dp_4f84f0e3 <- function(crs = 4326, bbox = NULL, timespan = NULL, halpern_layers = NULL, ...) {
  # Output folders and other objects used
  uid <- "4f84f0e3"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  exist <- check_files(uid, name, ondisk = FALSE)
  path <- make_output(uid, name)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!exist$raw) {
    # If the data is downloaded from online sources
    urls <- c(
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_artisanal_fishing_mol_20150714093005",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_demersal_destructive_fishing_mol_20150714093045",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_demersal_nondest_high_bycatch_mol_20150714093145",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_demersal_nondest_low_bycatch_mol_20150714093246",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_inorganic_mol_20150714093318",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_invasives_mol_20150714093329",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_night_lights_mol_20150714092417",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_ocean_acidification_mol_20150714093342",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_ocean_pollution_mol_20150714093406",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_oil_rigs_lzw_mol_20150714092427",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_pelagic_high_bycatch_mol_20150714093423",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_pelagic_low_bycatch_mol_20150714093503",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_plumes_fert_mol_20150714092433",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_plumes_pest_mol_20150714092439",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_population_mol_20150714092443",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_shipping_mol_20150714093538",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_sst_mol_20150714092501",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_uv_mol_20150714092816"
    )

    # User can select which data they want
    if (is.null(halpern_layers)) {
      halpern_layers <- c(
        "artisanal_fishing", "demersal_destructive_fishing", "demersal_destructive_fishing",
        "demersal_nondest_high_bycatch", "demersal_nondest_low_bycatch", "inorganic",
        "invasives", "night_lights", "ocean_acidification", "ocean_pollution", "oil_rigs_lzw",
        "pelagic_high_bycatch", "pelagic_low_bycatch", "plumes_fert", "plumes_pest",
        "population", "shipping", "sst", "uv"
      )
    }
    iid <- stringr::str_detect(urls, paste(halpern_layers, collapse = "|"))
    urls <- urls[iid]

    # Load
    pipeload(
      urls = urls,
      output = here::here(path, "raw"),
      large = TRUE
    )

    # Add zip extension to files and unzip
    files <- dir(here::here(path, "raw"), full.names = TRUE)
    lapply(
      files,
      function(x) file.rename(x, glue("{x}.zip"))
    )
    
    zipfiles <- glue("{x}.zip")
    lapply(
      zipfiles,
      function(x) {
        utils::unzip(
          x,
          exdir = here::here(path, "raw")
        )
      }
    )
  }
  # _________________________________________________________________________________________ #

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # To delete after processing
    newfiles <- dir(here::here(path, "raw"), full.names = TRUE)
    newfiles <- newfiles[!newfiles %in% zipfiles]

    # tif files to import
    files <- dir(here::here(path, "raw"), pattern = ".tif$", full.names = TRUE)
    dat <- lapply(files, stars::read_stars, proxy = TRUE)
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
      data_timespan = 1985:2006
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
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    if (!is.null(bbox)) {
      bbox <- bbox_poly(bbox, crs = 4326) |>
        sf::st_transform(crs = sf::st_crs(dat[[1]])) |>
        sf::st_bbox()
      dat <- lapply(dat, dp_parameters, bbox = bbox)
    }
    warning("WARNING: The Halpern datasets (id: 4f84f0e3) can be very large; hence the native spatial projection (EPSG: 54009) is kept rather than transformed. Remember to consider this for further analyses.")
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    name <- tools::file_path_sans_ext(basename(files))
    fm <- glue("{path}/{nm}-{name}.tif")
    for (i in 1:length(name)) {
      stars::write_stars(dat[[i]], fm[i])
    }
    # Delete decompressed file, as they are very big
    unlink(newfiles)

    # Metadata
    mt <- here::here(path, glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
