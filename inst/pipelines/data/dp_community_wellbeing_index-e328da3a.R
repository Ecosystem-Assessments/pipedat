#' @eval get_name("e328da3a")
#'
#' @eval get_description("e328da3a")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: e328da3a
#'
#' @examples
#' \dontrun{
#' dp_e328da3a()
#' }
dp_e328da3a <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "e328da3a"
  nm <- glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!exist$raw) {
    govcan <- get_pipeline(uid)$data_uuid
    pipeload(
      govcan = govcan,
      output = here::here(path, "raw"),
      large = FALSE
    )
  }
  # _________________________________________________________________________________________ #

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # File names
    files <- dir(here::here(path, "raw"), pattern = "_eng.csv")
    files <- files[!stringr::str_detect(files, "DICT_")]
    years <- substr(files, 5, 8) |>
      as.numeric()
    files <- files[!is.na(years)]
    years <- years[!is.na(years)]

    # Import and adjust column names
    modif <- data.frame(
      from = c(
        "CSD Code",
        "CSD Name",
        "2A Population",
        "Income",
        "Education",
        "Housing",
        "Labour Force Activity",
        "CWB",
        "Community Type",
        "Census Population",
        "GNR"
      ),
      to = c(
        "csd_code",
        "csd_name",
        "population",
        "income",
        "education",
        "housing",
        "labour_force_activity",
        "cwb",
        "community_type",
        "population",
        "gnr"
      )
    )

    dat <- list()
    for (i in 1:length(files)) {
      # -----
      dat[[i]] <- utils::read.csv(
        here::here(path, "raw", files[i]),
        header = FALSE,
        skip = 1
      )

      # -----
      colnm <- readLines(here::here(path, "raw", files[i]))[1] |>
        stringr::str_split(pattern = ",") |>
        unlist()
      colnm <- gsub(" /(.*)", "", colnm)
      for (j in 1:nrow(modif)) colnm <- gsub(modif$from[j], modif$to[j], colnm)
      colnames(dat[[i]]) <- colnm

      # -----
      dat[[i]] <- dplyr::mutate(
        dat[[i]],
        community_type = gsub(" /(.*)", "", community_type),
        year = years[i]
      )
    }

    dat <- dplyr::bind_rows(dat)
    # _________________________________________________________________________________________ #




    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_timespan = timespan,
      access = timestamp(),
      data_timespan = years
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
      timespan = timespan
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    fm <- here::here(path, glue("{nm}"))
    masterwrite(dat, fm)

    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}
