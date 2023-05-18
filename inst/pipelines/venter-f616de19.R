#' @eval get_name("f616de19")
#'
#' @eval get_description("f616de19")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: f616de19
#'
#' @examples
#' \dontrun{
#' dp_f616de19()
#' }
dp_f616de19 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ingrid = TRUE, ...) {
  uid <- "f616de19"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  path <- make_path(uid)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_raw(uid)) {
    # If the data is downloaded from online sources
    urls <- "https://datadryad.org/stash/downloads/file_stream/100086"
    pipeload(
      urls = urls,
      output = here::here(path, "raw"),
      large = TRUE
    )
    # Rename files
    file.rename(
      here::here(path, "raw", "100086"),
      here::here(path, "raw", "HumanFootprintv2.7z")
    )
  }
  # _________________________________________________________________________________________ #    
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (check_format(uid)) {
    archive::archive_extract(
      here::here(path, "raw", "HumanFootprintv2.7z"),
      dir = here::here(path, "raw", "HumanFootprint")
    )

    # Load all
    files <- dir(here::here(path, "raw", "HumanFootprint", "Dryadv3", "Maps"), pattern = ".tif$")
    dat <- lapply(
      here::here(path, "raw", "HumanFootprint", "Dryadv3", "Maps", files),
      masterload
    )
    datnames <- lapply(dat, names) |>
      unlist() |>
      tools::file_path_sans_ext()
    
    # Subset data (if specified by user)
    # on.exit(sf::sf_use_s2(TRUE), add = TRUE)
    # sf::sf_use_s2(FALSE)
    dat <- lapply(dat, dp_parameters, bbox = bbox, timespan = timespan)

    # Export
    fm <- here::here(path, "format", glue::glue("{nm}-{datnames}"))
    for (i in 1:length(fm)) masterwrite(dat[[i]], fm[i])
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
  clean_path(uid)
  # _________________________________________________________________________________________ #
}
