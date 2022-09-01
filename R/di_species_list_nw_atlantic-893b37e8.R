#' @eval get_name("893b37e8")
#'
#' @eval get_description("893b37e8")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 893b37e8
#'
#' @examples
#' \dontrun{
#' di_893b37e8()
#' }
di_893b37e8 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, ...) {
  # Output folders and other objects used
  uid <- "893b37e8"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  exist <- check_files(uid, name, ondisk = FALSE)
  path <- make_output(uid, name)

  if (!exist$integrated) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    raw_id <- get_rawid(uid) # String with data to import
    pipedat(raw_id, bbox, bbox_crs, timespan)
    dat <- importdat(raw_id)
    carms <- dat["carms_checklist-084860fd.csv"][[1]]
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ANALYZE / FORMAT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    obs_lists <- dat[
      c(
        "dfo_survey_4vsw-2aafec74-gscat.csv",
        "dfo_survey_fall-90e90110-gscat.csv",
        "dfo_survey_spring-21f8a758-gscat.csv",
        "dfo_survey_summer-3348d162-gscat.csv"
      )
    ]
    sp_lists <- dat[
      c(
        "dfo_survey_4vsw-2aafec74-gsspecies.csv",
        "dfo_survey_fall-90e90110-gsspecies.csv",
        "dfo_survey_spring-21f8a758-gsspecies.csv",
        "dfo_survey_summer-3348d162-gsspecies.csv"
      )
    ]

    for (i in 1:length(obs_lists)) {
      spfq <- table(obs_lists[[i]]$SPEC) |>
        as.data.frame() |>
        dplyr::mutate(Var1 = as.integer(as.character(Var1)))
      spid <- unique(obs_lists[[i]]$SPEC)
      lsid <- sp_lists[[i]]$CODE %in% spid
      sp_lists[[i]] <- sp_lists[[i]][lsid, ] |>
        dplyr::left_join(spfq, by = c("CODE" = "Var1"))
    }

    # -----
    species <- dplyr::bind_rows(sp_lists) |>
      dplyr::group_by(CODE, SPEC, COMM) |>
      dplyr::summarise(Freq = sum(Freq)) |>
      dplyr::arrange(SPEC) |>
      remove_code() |> # See helper function at the end
      eaMethods::review_taxa(field = "SPEC") |>
      dplyr::group_by(SPEC) |>
      dplyr::summarise(Freq = sum(Freq))

    
    # Get species AphiaID
    # uid <- worrms::wm_name2id_(name = species$SPEC)
    load("temp.rda")
    df <- data.frame(SPEC = names(uid), aphiaID = unlist(uid))
    species <- dplyr::left_join(species, df, by = "SPEC") |>
               dplyr::mutate(aphiaID = ifelse(aphiaID == -999, NA, aphiaID))
    
    
    verif <- dplyr::filter(species, is.na(aphiaID))
    as.data.frame(verif)

    # as.data.frame(sp)
    # taxize::classification()
    
    
    
    # # Try to get aphiaID
    # library(worrms)
    # dat$scientificName <- gsub(' sp\\.', '', dat$scientificName) # Add points between names
    # dat$scientificName <- stringr::str_trim(dat$scientificName, side = 'both') # Remove white spaces
    # sp <- sort(unique(dat$scientificName)) # Unique species
    # aphia <- wm_name2id_(name = sp) # Extract aphia ids from worms
    # sp <- data.frame(scientificName = sp, aphiaID = unlist(aphia)) # Data frame with species and aphiaID
    # message("WARNING: There are 19 species for which the aphia is not correct. To verify manually")
    # 
    
    
  

  #   # Taxonomy
  # library(taxize)
  # uid <- taxize::get_ids(species$SPEC[1:10], db = "eol")
  # classif <- classification(aphia$aphiaID, db = "worms")
  # 
  # # -----
  # x <- classif
  # nm <- names(classif)
  # for(i in 1:length(classif)) {
  #   x[[i]] <- as.data.frame(x[[i]]) %>%
  #             select(-id)
  # 
  #   # -----
  #   if ("species" %in% x[[i]]$rank) {
  #     gn <- x[[i]]$rank == "Genus"
  #     sp <- x[[i]]$rank == "Species"
  #     temp <- c(paste(x[[i]]$name[gn], x[[i]]$name[sp]), "ScientificName")
  #   } else {
  #     temp <- c(last(x[[i]]$name), "ScientificName")
  #   }
  #   x[[i]] <- rbind(x[[i]], temp)
  # 
  #   # -----
  #   x[[i]]$aphiaID <- nm[i]
  # }
  # 
  # # -----
  # class(x) <- "list"
  # x <- bind_rows(x)
  # 
  # # -----
  # x <- x %>%
  #      pivot_wider(id_cols = aphiaID,
  #                  names_from = rank,
  #                  values_from = name) %>%
  #      mutate(aphiaID = as.numeric(aphiaID)) %>%
  #      select(aphiaID, ScientificName, Kingdom, Phylum, Class, Order, Family, Genus, Species)
  # 
  # 
  # # -----
  # aphia <- left_join(aphia, x, by = "aphiaID")


    
    # [566] "Polychaeta c."
    # [567] "Polychaeta c.,large"
    # [568] "Polychaeta c.,small"
    # "Aristeidae f"
    # [494] "Crab"
    # [506] "Lithodes/neolithodes"    
    # " () "

    
    
    
    # # Check with CaRMS
    # species$SPEC %in% carms$ScientificName
    # 
    # "dfo_survey_4vsw-2aafec74-gscat.csv"
    # 
    # "dfo_survey_4vsw-2aafec74-gsspecies.csv"
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "integration",
      pipeline_id = uid,
      integration_data = raw_id
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
    fm <- here::here(path, glue("{nm}.csv"))
    utils::write.csv(dat, fm, row.names = FALSE)

    # Metadata
    mt <- here::here(path, glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  }
}

# CODE to remove, specific to Atlantic surveys
remove_code <- function(df) {
  remco <- c(
    1510, # Buccinidae eggs (259)
    1530, # Cephalopoda eggs (4)
    2507, # Crab (13)
    2001, # Crustacea larvae (1)
    8000, # Ctenophores,coelenterates,porifera (8)
    1100, # Eggs (17)
    100,  # Finfishes (6)
    1200, # Fish eggs-unidentified (11)
    9400, # Foreign articles,garbage (10)
    9301, # Fucus (13)
    1511, # Gasteropoda eggs (12)
    1218, # Hemitripterus americanus, eggs (1)
    1312, # Hyppolytid eggs (1)
    1600, # Invertebrate eggs (3)
    1701, # Marine invertebrata (1)
    1500, # Mollusca eggs (8)
    1228, # Myoxocephalus eggs (1)
    8366, # No longer used - phakellia (2)
    9630, # Organic debris (39)
    7000, # Parasites,round worms (1)
    4349, # Pectinidae shells (4)
    1199, # Purse barndoor skate (2)
    1203, # Purse little skate (37)
    1202, # Purse smooth skate (5)
    1201, # Purse thorny skate (8)
    1204, # Purse winter skate (24)
    1224, # Raja eggs (462)
    9999, # Reserved (14)
    3999, # Sand tube (2)
    8530, # Sea corals (24)
    592,  # Shark (1)
    2499, # Shrimp-like (1)
    8327, # Coral unidentified (130)
    9200, # Stones and rocks (27)
    9003, # Unid fish and eggs (1)
    9001, # Unid fish and invertebrates (3)
    9002, # Unid fish and remains (1)
    9000, # Unid remains,digested (26)
    90,   # Fish (13)
    9991, # Unidentified (341)
    9992, # Unidentified (132)
    9993, # Unidentified (89)
    9994, # Unidentified (68)
    9995, # Unidentified (55)
    9996, # Unidentified (42)
    9997, # Unidentified (29)
    9998, # Unidentified (25)
    1091, # Unidentified a (78)
    1092, # Unidentified b (20)
    1093, # Unidentified c (10)
    1094, # Unidentified d (10)
    1095  # Unidentified e (6)
  )
    
  iid <- df$CODE %in% remco
  df[!iid, ]
}