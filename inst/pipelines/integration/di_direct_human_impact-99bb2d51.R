#' @eval get_name("99bb2d51")
#'
#' @eval get_description("99bb2d51")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 99bb2d51
#'
#' @examples
#' \dontrun{
#' di_99bb2d51()
#' }
di_99bb2d51 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, ...) {
  # Output folders and other objects used
  uid <- "99bb2d51"
  nm <- glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

  if (!exist$integrated) {
    # WARNING: For R CMD CHECK
    DAUID <- DGUID <- Geographic.code <- Population..2016 <-
      Population.and.dwelling.counts..5...Population..2021..1. <-
      area <- area2 <- direct_human_impact <- population <-
      propArea <- propPop <- NULL
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    raw_id <- get_rawid(uid) # String with data to import
    pipedat(raw_id, bbox, bbox_crs, timespan)
    dat <- importdat(raw_id)
    bound2016 <- dat[["census_boundary_2016-c676dc2b.geojson"]]
    bound2021 <- dat[["census_boundary_2021-b9024b04.geojson"]]
    pop2016 <- dat[["census_population_2016-d147406d.csv"]]
    pop2021 <- dat[["census_population_2021-d96dec16.csv"]]

    # Study grid, if applicable
    if (is.null(grid)) {
      grid <- sf::st_read("data/data-grid/grid_poly.geojson", quiet = TRUE)
    }
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ANALYZE / FORMAT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Transform data in meters
    bound2016 <- sf::st_transform(bound2016, crs = 32198)
    bound2021 <- sf::st_transform(bound2021, crs = 32198)
    grid <- sf::st_transform(grid, crs = 32198)

    # Select only population data
    pop2016 <- dplyr::select(
      pop2016,
      DAUID = Geographic.code,
      population = Population..2016
    ) |>
      dplyr::mutate(DAUID = as.character(DAUID))

    pop2021 <- dplyr::select(
      pop2021,
      DGUID,
      population = Population.and.dwelling.counts..5...Population..2021..1.
    )

    # Join population data with dissemination area geometries
    bound2016 <- dplyr::left_join(bound2016, pop2016, by = "DAUID") |>
      dplyr::select(population)
    bound2021 <- dplyr::left_join(bound2021, pop2021, by = "DGUID") |>
      dplyr::select(population)

    # Select grid cells that are less than 2km from the coast and apply a 10km buffer
    # Use dissemination area geometries as coastline
    grid_buffer <- function(bound, grid) {
      iid <- sf::st_buffer(bound, 2000) |>
        sf::st_intersects(grid) |>
        unlist() |>
        unique() |>
        sort()
      sf::st_buffer(grid[iid, ], 10000)
    }
    coast2016 <- grid_buffer(bound2016, grid)
    coast2021 <- grid_buffer(bound2021, grid)

    # Select only dissemination areas that intersect with buffered grid cells
    bound_subset <- function(coast, bound) {
      iid <- sf::st_intersects(coast, bound) |>
        unlist() |>
        unique() |>
        sort()
      bound[iid, ]
    }
    bound2016 <- bound_subset(coast2016, bound2016)
    bound2021 <- bound_subset(coast2021, bound2021)

    # Intersection of each grid cell with overlapping dissemination areas and
    # recalculating the population as a function of the proportional area of
    # the dissimation area overlapping with grid cell
    prop_population <- function(coast, bound) {
      bound <- dplyr::mutate(bound, area = sf::st_area(bound)) |>
        sf::st_intersection(coast)

      dplyr::mutate(
        bound,
        area2 = sf::st_area(bound),
        propArea = area2 / area,
        propPop = as.numeric(population * propArea)
      ) |>
        sf::st_set_geometry(NULL) |>
        dplyr::select(uid, propPop) |>
        dplyr::group_by(uid) |>
        dplyr::summarise(direct_human_impact = sum(propPop)) |>
        dplyr::filter(direct_human_impact > 0)
    }
    dhi <- list()
    dhi[[1]] <- prop_population(coast2016, bound2016)
    dhi[[2]] <- prop_population(coast2021, bound2021)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "integration",
      pipeline_id = uid,
      integration_data = raw_id,
      integration_grid = get_grid_info(grid) # if applicable
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
    fm <- here::here(path, glue("{nm}-{c(2016,2021)}.csv"))
    for (i in 1:length(fm)) utils::write.csv(dhi[[i]], fm[i], row.names = FALSE)

    # Metadata
    mt <- here::here(path, glue("{nm}.yaml"))
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- here::here(path, glue("{nm}.bib"))
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  }
}
