#' @eval get_name("e2b7e6c4")
#'
#' @eval get_description("e2b7e6c4")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param fishing_intensity_metric type of fishing intensity metric to evaluate, one of: 1: Fishing effort density, no normalization; 2: Fishing effort density; 3: Fishing biomass yield density; 4: Fishing relative biomass yield density. Passed on to eaMethods::fishing_intensity().
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: e2b7e6c4
#'
#' @examples
#' \dontrun{
#' di_e2b7e6c4()
#' }
di_e2b7e6c4 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, fishing_intensity_metric = 3, ...) {
  # Output folders and other objects used
  uid <- "e2b7e6c4"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

  if (!exist$integrated) {
    # WARNING: For R CMD CHECK
    Categorie <- Codes <- DA_ESP <- DF_ESP <- DL_ESP <-
      ESP_STAT <- Freq <- Var1 <- date_cap <- gearClass <-
      latitude <- longitude <- mobility <- pd_deb <- NULL
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    raw_id <- get_rawid(uid) # String with data to import
    pipedat(raw_id, bbox, bbox_crs, timespan)
    dat <- importdat(raw_id)
    logbooks <- dat[["fisheries_logbooks-f2109e69.csv"]]
    gear <- dat[["fisheries_logbooks-f2109e69_gear.csv"]]
    species <- dat[["fisheries_logbooks-f2109e69_species.csv"]]
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ANALYZE / FORMAT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # =~-~=~-~=~-~=~-~=~-~= #
    # Classify gear types
    # -------------------
    # NOTE:
    #
    # Fishing activities are performed using a variety of gears types, e.g. trap,
    # trawl, dredge, driftnet, hand line, longline, scuba diving, purse seine, seine,
    # beach seine and jig fishing. Intensity of fishing activities was divided among
    # gear types and based on their respective types of environmental impacts.
    # Gear classification is done using the classification presented in Halpern
    # et al. (2008) and Halpern et al. (2015a) and is broken down into 5 distinct
    # classes:
    #
    #  - demersal destructive (DD),
    #  - demersal, non-destructive, low-bycatch (DNL),
    #  - demersal, non-destructive, high-bycatch (DNH),
    #  - pelagic, low-bycatch (PLB),
    #  - pelagic, high-bycatch (PHB),
    # =~-~=~-~=~-~=~-~=~-~= #
    # Select codes
    # Run gear[gear$Codes %in% dd, ] to see which gear class is in which category
    l <- list(
      data.frame(gearClass = "DD", Codes = c(6, 9, 10, 11, 12, 16, 19, 71, 72, 72, 74, 77, 93)),
      data.frame(
        gearClass = "DNH",
        Codes = c(3, 21, 22, 24, 25, 33, 45, 46, 47, 61, 62, 63, 65, 66, 67, 68, 69, 78, 79, 80, 84, 86, 87, 88, 89, 92, 98)
      ),
      data.frame(gearClass = "DNL", Codes = c(70, 75, 81, 83, 85, 91, 94, 96)),
      data.frame(
        gearClass = "PHB",
        Codes = c(4, 5, 13, 14, 15, 17, 18, 27, 28, 29, 30, 32, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 48, 50, 51, 52, 64)
      ),
      data.frame(gearClass = "PLB", Codes = c(31, 53, 54, 55, 56, 58, 59, 60)),
      data.frame(gearClass = NA, Codes = c(0, 1, 2, 57, 73, 76, 90, 95, 99, 99))
    )
    gear <- dplyr::left_join(gear, dplyr::bind_rows(l), by = "Codes")

    # =~-~=~-~=~-~=~-~=~-~= #
    # Classify gear mobility
    # ----------------------
    # NOTE:
    #
    # Gear types can also be further classified into fixed or mobile engines based
    # on their mobility. We used these two mobility classes to generate a buffer of
    # impact around each fishing activity coordinates to consider potential spatial
    # uncertainty associated with locations and the fact that mobile engines can be
    # tracted over several kilometers during fishing activities and that we do not
    # have the beginning and end points of mobile fishing events. Buffer sizes for
    # fixed (F) and mobile (M) engine is of 200 and 2000 meters, respectively.
    #
    # The index holds the mobility information for almost all gear types, but some
    # are missing. We manually add them here. Categories 81 and 85 were also changed
    # from mobile to fixed for the purposes of our analysis, even though it does not
    # affect the actual data we are working with because there are no observations of
    # those gear types in the study area for this project.
    # =~-~=~-~=~-~=~-~=~-~= #
    m <- c(9, 71, 95)
    f <- c(45, 46, 47, 70, 76, 83, 94, 81, 85)
    na <- 0
    gear$Categorie[gear$Code %in% m] <- "M"
    gear$Categorie[gear$Code %in% f] <- "F"
    gear$Categorie[gear$Code %in% na] <- NA

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Classify and filter fisheries data
    # ----------------------------------
    gear <- dplyr::select(gear, Codes, gearClass, mobility = Categorie)

    # -----
    logbooks <- dplyr::left_join(logbooks, gear, by = c("engin" = "Codes")) |>
      dplyr::filter(!is.na(gearClass)) |>
      dplyr::filter(!is.na(mobility))

    # -----
    # LBS to KG
    pdid <- logbooks$un_mes == "P"
    logbooks$pd_deb[pdid] <- logbooks$pd_deb[pdid] * 0.453592

    # -----
    logbooks <- unique(logbooks)

    # ------------------------------------------------------------
    # NOTE: For metadata
    species_cible <- sort(unique(logbooks$prespvis))
    species <- table(logbooks$cod_esp) |>
      as.data.frame() |>
      dplyr::rename(ESP_STAT = Var1) |>
      dplyr::mutate(ESP_STAT = as.numeric(as.character(ESP_STAT))) |>
      dplyr::left_join(species, by = "ESP_STAT") |>
      dplyr::select(
        ID = ESP_STAT, Scientific = DL_ESP, Espece = DF_ESP,
        Species = DA_ESP, Freq
      )

    gear_freq <- table(logbooks$gearClass) |>
      as.data.frame()
    # ------------------------------------------------------------

    # -----
    logbooks <- dplyr::group_by(logbooks, date_cap, latitude, longitude, gearClass, mobility) |>
      dplyr::summarise(catch = sum(pd_deb))

    # Create spatial object
    logbooks <- sf::st_as_sf(logbooks, coords = c("longitude", "latitude"), crs = 4326)

    # -----
    logbooks <- sf::st_transform(logbooks, crs = 32198)
    fix <- logbooks[logbooks$mobility == "F", ] |>
      sf::st_buffer(200)

    mob <- logbooks[logbooks$mobility == "M", ] |>
      sf::st_buffer(2000)

    logbooks <- dplyr::bind_rows(fix, mob) |>
      sf::st_transform(crs = 4326)

    # -----
    logbooks <- logbooks |>
      dplyr::arrange(as.Date(date_cap))

    # Let's try something to try to not run into memory issues. It will be inefficient, but as long 
    # as the job gets done under 24h, I don't truly care
    years <- format(as.Date(logbooks$date_cap), format = "%Y")
    year_id <- unique(years) |> sort() 
    year_id <- year_id[year_id != "1999"]
    gearClass <- logbooks$gearClass
    gear_id <- unique(gearClass) |> sort() 
    iid <- expand.grid(gear_id, year_id, stringsAsFactors = FALSE)
    iids <- apply(iid, 1, function(x) which(gearClass == x[1] & years == x[2]))
    name <- glue::glue("{nm}-{iid[,1]}-{iid[,2]}")
    
    # Seperately subset dataset to keep this out of the function. 
    # Might be useless, but I'm trying to figure out where the code is breaking in terms of memory.
    # If this works I'll keep it like this, it will not really matter as long as it works
    l <- list()
    for(i in 1:length(iids)) l[[i]] <- logbooks[iids[[i]], ]  
        
    # Rasterize data
    grid <- stars::read_stars("data/grid/grid.tif", quiet = TRUE)
    bb <- sf::st_bbox(grid)
    r <- raster::raster(
      xmn=bb$xmin, 
      xmx=bb$xmax, 
      ymn=bb$ymin, 
      ymx=bb$ymax, 
      crs = 4326, 
      resolution = 0.02
    )
    m <- list()
    for(i in 1:length(l)) {
      m[[i]] <- fasterize::fasterize(
        l[[i]], 
        r, 
        field = "catch", 
        fun="sum"
      ) |>
      stars::st_as_stars() |>
      masteringrid(grid)
    }
    names(m) <- name
    
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "integration",
      pipeline_id = uid,
      integration_data = raw_id
    )

    # To add additional metadata for queried data
    meta <- add_metadata(meta,
      dataDescription = data.frame(
        accronyme = c("DD", "DNL", "DNH", "PLB", "PHB"),
        english = c(
          "Demersal, destructive, high-bycatch",
          "Demersal, non-destructive, low-bycatch",
          "Demersal, non-destructive, high-bycatch",
          "Pelagic, low-bycatch",
          "Pelagic, high-bycatch"
        ),
        description = c(
          "Commercial fishing activities using demersal fishing gear that may damage habitats or substrate, e.g. trawling and dragging.",
          "Commercial fishing activities using demersal fishing gear with little or no bycatch and not causing habitat modification, e.g., deep-sea fishing.",
          "Commercial fishing activities using demersal fishing gear with high bycatch and not causing habitat modification, e.g., trap and seine.",
          "Commercial fishing activities using pelagic fishing gear with little or no bycatch and not causing habitat modification, e.g., line fishing, purse seine.",
          "Commercial fishing activities using pelagic fishing gear with high bycatch and not causing habitat modification, e.g., gillnet and longline."
        )
      ),
      species = list(
        species_target = species_cible,
        species_capture = species
      ),
      gear = gear_freq
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
    fm <- here::here(path, glue::glue("{nm}-{iid$Var1}-{iid$Var2}"))
    for(i in 1:length(m)) masterwrite(m[[i]], fm[i])
    
    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)  
    # _________________________________________________________________________________________ #
  }
}
