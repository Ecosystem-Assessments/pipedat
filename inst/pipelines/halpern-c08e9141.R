#' @eval get_name("c08e9141")
#'
#' @eval get_description("c08e9141")
#'
#' @eval dp_params()
#' @param halpern_years years of data to load, choices are 2008 and 2013
#' @param halpern_layers name of layers to download from the Halpern dataset. Possible entries are: "artisanal_fishing","demersal_destructive_fishing","demersal_destructive_fishing","demersal_nondest_high_bycatch","demersal_nondest_low_bycatch","inorganic","invasives","night_lights","ocean_acidification","ocean_pollution","oil_rigs","pelagic_high_bycatch","pelagic_low_bycatch","plumes_fert","plumes_pest","population","shipping","slr","sst","uv".
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: c08e9141
#'
#' @examples
#' \dontrun{
#' dp_c08e9141()
#' }
dp_c08e9141 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, keep_raw = TRUE, halpern_years = NULL, halpern_layers = NULL, ...) {
  uid <- "c08e9141"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  urls <- data.frame(
    urls = c(
      # 2008
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
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_uv_mol_20150714092816",
      
      # 2013
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_artisanal_fishing_mol_20150714095433",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2008_demersal_destructive_fishing_mol_20150714093559",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_demersal_nondest_high_bycatch_mol_20150714093620",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_demersal_nondest_low_bycatch_mol_20150714093643",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_inorganic_mol_20150714095441",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_invasives_mol_20150714093653",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_night_lights_mol_20150714093709",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_ocean_acidification_mol_20150714095453",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_ocean_pollution_mol_20150714093808",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_oil_rigs_lzw_mol_20150714093841",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_pelagic_high_bycatch_mol_20150714093845",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_pelagic_low_bycatch_mol_20150714093900",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_plumes_fert_mol_20150714093914",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_plumes_pest_mol_20150714093921",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_population_mol_20150714093926",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_shipping_mol_20150714094045",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_slr_mol_20150714094636",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_sst_mol_20150714094924",
      "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/raw_2013_uv_mol_20150714095238"
    ),
    years = c(rep(2008, 18), rep(2013, 19)),
    names = c(
      # 2008
      "artisanal_fishing", "demersal_destructive_fishing",
      "demersal_nondest_high_bycatch", "demersal_nondest_low_bycatch", "inorganic",
      "invasives", "night_lights", "ocean_acidification", "ocean_pollution", "oil_rigs",
      "pelagic_high_bycatch", "pelagic_low_bycatch", "plumes_fert", "plumes_pest",
      "population", "shipping", "sst", "uv",
      # 2013
      "artisanal_fishing", "demersal_destructive_fishing",
      "demersal_nondest_high_bycatch", "demersal_nondest_low_bycatch", "inorganic",
      "invasives", "night_lights", "ocean_acidification", "ocean_pollution", "oil_rigs",
      "pelagic_high_bycatch", "pelagic_low_bycatch", "plumes_fert", "plumes_pest",
      "population", "shipping", "slr", "sst", "uv"
    )
  )

  # User can select which data they want
  if (is.null(halpern_years)) {
    halpern_years <- c(2008, 2013)
  }
  if (is.null(halpern_layers)) {
    halpern_layers <- c(
      "artisanal_fishing", "demersal_destructive_fishing", "demersal_destructive_fishing",
      "demersal_nondest_high_bycatch", "demersal_nondest_low_bycatch", "inorganic",
      "invasives", "night_lights", "ocean_acidification", "ocean_pollution", "oil_rigs",
      "pelagic_high_bycatch", "pelagic_low_bycatch", "plumes_fert", "plumes_pest",
      "population", "shipping", "slr", "sst", "uv"
    )
  }
  iid <- urls$years %in% halpern_years & urls$names %in% halpern_layers
  urls <- urls[iid, ]

  if (check_raw(uid)) {
    # Load
    pipeload(
      urls = urls$urls,
      output = here::here(path, "raw"),
      large = TRUE
    )

    # Add zip extension to files and unzip
    files <- dir(here::here(path, "raw"), full.names = TRUE)
    lapply(
      files,
      function(x) file.rename(x, glue::glue("{x}.zip"))
    )
  }
  # _________________________________________________________________________________________ #    
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_format(uid)) {
    # Unzip
    zipfiles <- basename(glue::glue("{urls$urls}.zip"))
    if (2008 %in% halpern_years) {
      dir.create(here::here(path, "raw", "2008"))
      iid <- which(urls$years == 2008)
      for (i in iid) {
        utils::unzip(
          here::here(path, "raw", zipfiles[i]),
          exdir = here::here(path, "raw", "2008")
        )
      }
    }
    if (2013 %in% halpern_years) {
      dir.create(here::here(path, "raw", "2013"))
      iid <- which(urls$years == 2013)
      for (i in iid) {
        utils::unzip(
          here::here(path, "raw", zipfiles[i]),
          exdir = here::here(path, "raw", "2013")
        )
      }
    }

    # tif files to import
    files <- c(
      dir(here::here(path, "raw", "2008"), pattern = ".tif$", full.names = TRUE),
      dir(here::here(path, "raw", "2013"), pattern = ".tif$", full.names = TRUE)
    )
    dat <- lapply(files, stars::read_stars, proxy = TRUE)

    # Subset data (if specified by user)
    # on.exit(sf::sf_use_s2(TRUE), add = TRUE)
    # sf::sf_use_s2(FALSE)
    dat <- lapply(dat, dp_parameters, bbox = bbox, timespan = timespan)
    
    # Export
    name <- tools::file_path_sans_ext(basename(files))
    fm <- here::here(path,"format",glue::glue("{nm}-{name}-{urls$years}"))
    for (i in 1:length(name)) masterwrite(dat[[i]], fm[i])
  } 
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Integrate data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_ingrid(uid) & ingrid) {
    # Import & integrate
    dat <- importdat(uid, "format")
    dat <- lapply(dat, masteringrid)
    name <- names(dat)
    name <- gsub("halpern_cea-4f84f0e3-", "", name)
    name <- gsub(".tif", "", name)
    
    # Export 
    fm <- glue::glue("{nm}-{name}")
    for(i in 1:length(fm)) masterwrite(dat[[i]], here::here(path, "ingrid", fm[i]))
  }
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Metadata & bibtex
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Metadata
  meta <- get_metadata(
    pipeline_type = "data",
    pipeline_id = uid,
    pipeline_bbox = bbox, 
    pipeline_timespan = timespan, 
    access = timestamp()
  )
  
  # bibtex
  bib <- get_bib(uid)
  
  # Export
  mt <- here::here(path, nm)
  masterwrite(meta, mt)
  masterwrite(bib, mt)  
  write_pipeline(uid)
  
  # Clean 
  clean_path(uid, keep_raw)
  # _________________________________________________________________________________________ #
}
