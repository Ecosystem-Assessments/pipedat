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
      eaMethods::clean_taxa(field = "SPEC") |>
      change_taxa_name() |> # See helper function at the end
      eaMethods::review_taxa(field = "SPEC") |>
      dplyr::group_by(SPEC) |>
      dplyr::summarise(Freq = sum(Freq)) |>
      eaMethods::get_aphia(field = "SPEC") |>
      eaMethods::get_classification()
    # masterwrite(species,"temp_species")
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
    utils::write.csv(species, fm, row.names = FALSE)

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
    2529, # Calappa megalops (1)
    1530, # Cephalopoda eggs (4)
    290, # Clupeidae/osmeridae (1)
    2507, # Crab (13)
    2001, # Crustacea larvae (1)
    8000, # Ctenophores,coelenterates,porifera (8)
    1100, # Eggs (17)
    100, # Finfishes (6)
    1200, # Fish eggs-unidentified (11)
    9400, # Foreign articles,garbage (10)
    9301, # Fucus (13)
    1511, # Gasteropoda eggs (12)
    8363, # Halipterus (4)
    1218, # Hemitripterus americanus, eggs (1)
    2810, # Hyperia (2)
    2802, # Hyperia oculata (1)
    1312, # Hyppolytid eggs (1)
    1600, # Invertebrate eggs (3)
    2525, # Lithodes/neolitodes (31)
    4514, # Loliginidae,ommastrephidae (55)
    1701, # Marine invertebrata (1)
    1500, # Mollusca eggs (8)
    1228, # Myoxocephalus eggs (1)
    4233, # Nassa bivittata (3)
    4235, # Nassariidae thaisidae (1)
    8366, # No longer used - phakellia (2)
    8355, # Octopoda, cirrata (3)
    9630, # Organic debris (39)
    7000, # Parasites,round worms (1)
    4349, # Pectinidae shells (4)
    4310, # Protobranchia, heterodonta (70)
    1199, # Purse barndoor skate (2)
    1203, # Purse little skate (37)
    1202, # Purse smooth skate (5)
    1201, # Purse thorny skate (8)
    1204, # Purse winter skate (24)
    1224, # Raja eggs (462)
    9999, # Reserved (14)
    3999, # Sand tube (2)
    8530, # Sea corals (24)
    592, # Shark (1)
    2499, # Shrimp-like (1)
    8327, # Coral unidentified (130)
    9200, # Stones and rocks (27)
    9300, # Thallophyta (49)
    9003, # Unid fish and eggs (1)
    9001, # Unid fish and invertebrates (3)
    9002, # Unid fish and remains (1)
    9000, # Unid remains,digested (26)
    90, # Fish (13)
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
    1095 # Unidentified e (6)
  )

  iid <- df$CODE %in% remco
  df[!iid, ]
}


# Change names
change_taxa_name <- function(df) {
  dat <- rbind(
    c("Henrica", "Henricia"),
    c("Ophiura sarsi", "Ophiura sarsii"),
    c("Acanthephyra exemia", "Acanthephyra eximia"),
    c("Aristaepsis edwardsinana", "Aristaeopsis edwardsiana"),
    c("Arrhis phyllonix", "Arrhis phyllonyx"),
    c("Arrhoges occidentali", "Arrhoges occidentalis"),
    c("Astrotecten duplicatus", "Astropecten duplicatus"),
    c("Calathura branchiata", "Calathura brachiata"),
    c("Ceremaster granularis", "Ceramaster granularis"),
    c("Chionoecetes opili", "Chionoecetes opilio"),
    c("Evermanella indica", "Evermannella indica"),
    c("Glycera", "Ichnopus"),
    c("Gonatus steenstrupii", "Gonatus steenstrupi"),
    c("Gorgonocephalidae,asteronychidae", "Gorgonocephalus"),
    c("Omosudis lowei", "Omosudis lowii"),
    c("Paralepis atlantica kroyer", "Magnisudis atlantica"),
    c("Pitar morrhuana", "Agriopoma morrhuanum"),
    c("Porania pulvilis", "Porania pulvillus"),
    c("Poraniomorpha borealis", "Poraniomorpha hispida"),
    c("Sabinea sarsi", "Sabinea sarsii"),
    c("Terebratulina septentrionali", "Terebratulina septentrionalis"),
    c("Velutina laevigata", "Velutina velutina")
  )
  dat <- data.frame(from = dat[, 1], to = dat[, 2])
  for (i in 1:nrow(dat)) {
    iid <- df$SPEC %in% dat$from[i]
    df$SPEC[iid] <- dat$to[i]
  }
  df
}
