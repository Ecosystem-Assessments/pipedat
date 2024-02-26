#' Generate figures of the pipedat project
#'
#' This function creates a figure of the area of interest delineated by the study grid, and a figure for all data integrated in the study grid
#'
#' @return This function exports the figure of the area of interest in `figures/pipedat/aoi/` and the gridded figures in `figures/pipedat/ingrid/` in `png` format
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate report
#' pipeplot()
#' }
pipeplot <- function(res = 300, width = 225, height = 200, pal = viridis::viridis) {
  # Area of interest
  plotgrid(res, width, height, pal)

  # Gridded data
  plotingrid(res, width, height, pal)
}

plotgrid <- function(res, width, height, pal) {
  # Load grid
  grd <- here::here("project-data/grid/grid.tif") |>
    stars::read_stars()

  # Folders
  path <- here::here("figures", "pipedat", "aoi")
  chk_create(path)

  # Output arguments
  grDevices::png(
    here::here(path, "aoi.png"),
    res = res,
    width = width,
    height = height,
    units = "mm"
  )

  # Margins
  par(mar = c(1, 0, 5, 0))

  # Plot grid with base stars functionalities
  image(grd, col = pal(1), main = "Area of interest")

  # Plot canadian outline
  can <- basemap$can |>
    sf::st_transform(sf::st_crs(grd)) |>
    sf::st_geometry()
  plot(
    can,
    col = "#57575733",
    border = "#575757",
    lwd = .5,
    add = TRUE
  )

  # Close graphics device
  grDevices::dev.off()
}


plotingrid <- function(res, width, height, pal) {
  # Locate files
  files <- list.dirs(here::here("project-data", "pipedat"), full.names = TRUE)
  iid <- stringr::str_detect(files, "ingrid")
  files <- files[iid]

  if (length(files) > 0) {
    # Folders
    path <- here::here("figures", "pipedat", "ingrid")
    chk_create(path)

    # Load grid (for reference)
    grd <- here::here("project-data/grid/grid.tif") |>
      stars::read_stars()

    # Canadian outline
    can <- basemap$can |>
      sf::st_transform(sf::st_crs(grd)) |>
      sf::st_geometry()

    # All folders in a loop
    for (i in 1:length(files)) {
      # Load metadata
      meta <- dir(here::here(files[i], ".."), full.names = TRUE, pattern = "yaml") |>
        yaml::read_yaml()

      # Filenames
      filenames <- meta$ingrid$files

      for (j in 1:length(filenames$filenames)) {
        # Name
        nm <- filenames$filenames[j]
        sub <- filenames$names[j]

        # Load data
        dat <- here::here(files[i], glue::glue("{nm}.tif")) |>
          stars::read_stars()

        # Output arguments
        grDevices::png(
          here::here(path, glue::glue("{nm}.png")),
          res = res,
          width = width,
          height = height,
          units = "mm"
        )

        # Margins
        par(mar = c(1, 0, 5, 0))

        # Plot grid with base stars functionalities
        image(dat, col = pal(100), main = meta$description$name)

        # Subtext
        mtext(side = 3, text = sub)

        # Plot canadian outline
        plot(
          can,
          col = "#57575733",
          border = "#575757",
          lwd = .5,
          add = TRUE
        )

        # Close graphics device
        grDevices::dev.off()
      }
    }
  }
}
