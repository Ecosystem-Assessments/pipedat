#' @eval get_name("7a5323bb")
#'
#' @eval get_description("7a5323bb")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 7a5323bb
#'
#' @examples
#' \dontrun{
#' dp_7a5323bb()
#' }
dp_7a5323bb <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "7a5323bb"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

    
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!exist$raw) {
    pipedat("7c150fc3")
    pipedat("893b37e8")
  }
  # _________________________________________________________________________________________ #
    
  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Function to get children
    get_children <- function(dat, rows = NA) {
      dat <- lapply(dat, function(x) try(taxize::children(x, db = "worms", rows = rows))) |>
             purrr::discard(function(x) class(x) == "try-error") |>
             purrr::discard(function(x) plyr::empty(x[[1]]) | rlang::is_empty(x))
      nm <- lapply(dat, names) |> unlist()
      for(i in 1:length(dat)) {
        dat[[i]] <- dat[[i]][[1]] |>
                    dplyr::mutate(search = nm[i])
      }
      dat <- dplyr::bind_rows(dat)
      if (!is.na(rows)) {
      # Remove genus that do not match with search term (because of automatic row selection)
        iid <- apply(dat, 1, function(x) stringr::str_detect(x["childtaxa_name"], x["search"])) |>
               unlist()
        dat <- dat[iid, ] |>
               dplyr::select(-search)
      }
      dat
    }
    
    # Get GloBI interactions 
    get_globi <- function(x, inter) {
      rem <- c("hasEndoparasite","hasEctoparasite","parasiteOf")
      if (inter == "preyof") interaction_type <- c("eatenBy", "preyedUponBy")
      if (inter == "predof") interaction_type <- c("eats", "preysOn")
      
      # Get interactions & format
      otherkeys <- list("limit"=10000)
      
      # Divide in batches of 100
      st <- 100
      nsp <- length(x)
      if (nsp > st) {
        batch <- data.frame(
          from = seq(1, nsp, by = st),
          to = c(seq(st+1, nsp, by = st), nsp)
        )        
      } else {
        batch <- data.frame(from = 1, to = nsp)
      }
      
      
      for(i in 1:nrow(batch)) {
        int <- rglobi::get_interactions(
          x[batch$from[i]:batch$to[i]],
          interaction.type = interaction_type, 
          otherkeys = otherkeys
        ) 
      
        if (nrow(int) > 0) {
          int <- dplyr::filter(int, !interaction_type %in% rem) |>
                 dplyr::filter(!is.na(target_taxon_path)) |>
                 dplyr::mutate(
                   target_taxon_name = stringr::str_trim(target_taxon_name, side = "both")
                 ) 

          # To select only "species". Rough and prone to error, not very robust
          iid <- lapply(int$target_taxon_name, function(x) stringr::str_detect(x, " ")) |> unlist()
          int <- int[iid, ]
          
          # Binary interactions
          if (inter == "preyof") {
            out <- here::here(path,"raw","preyof.csv")
            int <- dplyr::select(int, predator = target_taxon_name, prey = source_taxon_name) |>
                   dplyr::distinct()
                   
            if (!file.exists(out)) {
              write.csv(int, file = out, row.names = FALSE)
            } else {
              temp <- read.csv(out)
              int <- dplyr::bind_rows(temp, int) |>
                     dplyr::distinct()
              write.csv(int, file = out, row.names = FALSE)
            }
          } else if (inter == "predof") {
            out <- here::here(path,"raw","predof.csv")
            int <- dplyr::select(int, predator = source_taxon_name, prey = target_taxon_name) |>
                   dplyr::distinct()
            if (!file.exists(out)) {
              write.csv(int, file = out, row.names = FALSE)
            } else {
              temp <- read.csv(out)
              int <- rbind(temp, int) |>
                     dplyr::distinct()
              write.csv(int, file = out, row.names = FALSE)
            }
          }
        }
      }
    }
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Get all species available in worms
    mmsb <- pipedat::importdat("7c150fc3")[[1]]
    msp <- pipedat::importdat("893b37e8")[[1]] |>
           dplyr::select(-SPEC, -Freq)
    dat <- dplyr::bind_rows(mmsb, msp)
    family <- sort(unique(dat$Family))
    genus <- sort(unique(dat$Genus))
    species <- sort(unique(dat$ScientificName))

    # Get all genera and species from families in dataset
    genus.add <- get_children(family)
    species.add <- c(genus, genus.add$childtaxa_name) |> unique() |> sort() |>
                   get_children(rows = 1) |>
                   dplyr::filter(childtaxa_rank == "Species") |>
                   dplyr::select(aphiaID = childtaxa_id, ScientificName = childtaxa_name) |>
                   rbind(dat[,c("aphiaID","ScientificName")]) |>
                   dplyr::arrange(ScientificName) |>
                   dplyr::select(ScientificName) |>
                   dplyr::distinct() |>
                   dplyr::rename(species = ScientificName)
                   
    # Get interactions from GloBI
    get_globi(species.add$species, inter = "preyof")
    get_globi(species.add$species, inter = "predof")
    preyof <- read.csv(here::here(path,"raw","preyof.csv"))
    predof <- read.csv(here::here(path,"raw","predof.csv"))
    interactions <- dplyr::bind_rows(preyof, predof) |>
                    dplyr::distinct()

    # Get aphia and taxonomy 
    # Limit to worms, as I am working only with marine species for this
    species <- data.frame(species = c(interactions$predator, interactions$prey)) |>
               dplyr::distinct() |>
               dplyr::arrange(species) 

    # Remove special characters
    iid <- gsub(" ","",species$species) |>
           stringr::str_detect("[^A-Za-z]")
    species$aphia <- NA # Don't ask...
    
    # Get aphia
    species <- species[!iid,] |>
               eaMethods::get_aphia("species") 
               
    # Get classification
    classif <- dplyr::select(species, -aphia) |>
               na.omit() |>
               eaMethods::get_classification()               
    
    # Remove binary interactions involving species for which taxonomy is not available
    iid <- apply(interactions, 1, function(x) all(x %in% classif$species))
    interactions <- interactions[iid, ]
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      access = timestamp()
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #
    
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT 
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data   
    fm <- here::here(path,glue::glue("{nm}-{c('interactions','species')}"))
    masterwrite(interactions, fm[1])
    masterwrite(classif, fm[2])
    
    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)  
    # _________________________________________________________________________________________ #
  } #if exist clean, don't run again
}
