#' plot pipedat data maps
#'
#' base plot functions for pipedat package
#'
#' @param dat object of class sf
#' @param bbox bounding box of the plot, should be of the form `c(xmin, ymin, xmax, ymax)`
#' @param main main title
#' @param type secondary title / data type (see metadata files)
#' @param subtitle subtitle
#' @param unit_data units of data
#' @param references data citation used for integrated data
#' @param ... further specifications, see \link{plot} and details.
#'
#' @export

pipeplot <- function(dat, ...) {
  UseMethod("pipeplot", dat)
}

#' @method pipeplot sf
#' @name pipeplot
#' @export
pipeplot.sf <- function(dat, bbox, main = NULL, type = NULL, subtitle = NULL, unit_data = NULL, references = NULL, ...) {

  # pdf(glue('./figures/figures-format/{data_id}.pdf'), width = 7, height = 5, pointsize = 12)
  # png(glue('./figures/delete.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)

  # -----------------------------------------------------
  # layout
  graphics::layout(
    matrix(c(2, 1), nrow = 2),
    heights = c(.2, .8)
  )

  # -----------------------------------------------------
  # Main plot
  par(family = "serif", mar = c(.5, .5, 0, .5))
  pal <- colorRampPalette(viridis::viridis(100))

  graphicsutils::plot0(
    x = c(bbox["xmin"], bbox["xmax"]),
    y = c(bbox["ymin"], bbox["ymax"])
  )
  graphics::box()

  # ------------------
  # Legend & colors
  bin <- dat[, 1, drop = TRUE] |>
    table() |>
    names()

  if (length(bin) == 2 | length(bin) == 1) {
    cols <- "#44c79f"
  } else {
    maxDat <- max(dat[, 1, drop = TRUE], na.rm = TRUE)
    cols <- pal(101)[((dat[, 1, drop = TRUE] / maxDat) * 100) + 1]
  }

  # Add sources
  if (!is.null(references)) {
    refs <- stringr::str_split(references, ",") |> unlist()
    txt <- glue::glue("Raw data : {references}. Details : Appendix 1.")
    graphics::mtext(
      text = txt,
      side = 1,
      font = 3,
      adj = .98,
      cex = .4,
      line = -.45
    )
  }


  # ------------------
  # Data
  plot(
    sf::st_geometry(dat),
    lwd = .25,
    add = TRUE,
    pch = 20,
    cex = .25,
    col = cols,
    border = cols
  )

  # Coastline
  if (sf::st_crs(basemap$can) != sf::st_crs(dat)) {
    sf::st_transform(basemap$can, crs = sf::st_crs(dat))
  }
  plot(
    sf::st_geometry(basemap$can),
    lwd = .5,
    border = "#575757",
    add = TRUE
  )


  # -----------------------------------------------------
  # Titles
  par(family = "serif", mar = c(0, 0, 0, 0))
  graphicsutils::plot0()

  # ------------------
  # Text
  y <- 1
  x <- -1
  yG <- .55
  if (!is.null(main)) {
    y <- y - yG
    graphics::text(
      x = x,
      y = y,
      labels = main,
      font = 2,
      adj = c(0, .5),
      cex = .75
    )
  }

  # Legend
  minUp <- .5
  if (length(bin) == 2 | length(bin) == 1) {
    sbt <- "Presence"
    plot_legend_bin(
      col = cols,
      subTitle = sbt,
      cexSub = .5,
      minUp = minUp
    )
  } else {
    plot_legend_cont(
      range = range(dat[, 1, drop = TRUE], na.rm = TRUE),
      pal = pal,
      subTitle = unit_data,
      cexSub = .4,
      minUp = minUp
    )
  }
}

#' Plot legend
#'
#' Function to create a legend
#'
#' @rdname plot_legend
#'
#' @export
#'
#' @param range numeric, vector with minimal and maximal values
#' @param pal character, vector of colors, or color palette
#' @param col character, single color to use for binary data
#' @param cexMain numeric, cex for legend main text
#' @param cexSub numeric, cex for legend subtitle text
#' @param mainTitle character, type of legend. Choices are 'continuous', 'binary' or 'categorical'
#' @param subTitle character, type of legend. Choices are 'continuous', 'binary' or 'categorical'
#' @param n numeric, number of ticks in the legend
#' @param minUp numeric, minimum upper side to write as a function of bbox extent
#'
#' @return Opens a graphical interface with the plot
#'
#' @keywords plot, legend
plot_legend_cont <- function(range = c(0, 1),
                             pal = NULL,
                             cexMain = 1,
                             cexSub = .75,
                             minUp = .175,
                             mainTitle = NULL,
                             subTitle = NULL,
                             n = 5) {
  # Legends
  # Palette
  if (class(pal) == "character") {
    pal <- colorRampPalette(pal)
  }

  # Determine plot boundaries, in units of the data
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  xR <- xmax - xmin
  yR <- ymax - ymin

  xinit <- xmin + .05 * xR # minimum left side to write
  yinit <- ymax - minUp * yR # minimum upper side to write
  ygap <- .15 * yR
  xgap <- .014 * xR
  ybarUp <- yinit - ygap / 2 - .0041 * yR
  ybarDn <- yinit - ygap - ygap / 2 + .0041 * yR

  # Plot
  x <- seq(from = xinit, to = xinit + .17 * xR, by = .0003 * xR)
  z <- data.frame(
    y1 = ybarUp,
    y2 = ybarDn,
    x1 = x[1:length(x) - 1],
    x2 = x[2:length(x)],
    col = pal(length(x) - 1),
    stringsAsFactors = F
  )
  for (k in 1:nrow(z)) {
    graphics::polygon(
      x = c(z$x1[k], z$x2[k], z$x2[k], z$x1[k], z$x1[k]),
      y = c(z$y1[k], z$y1[k], z$y2[k], z$y2[k], z$y1[k]),
      col = z$col[k],
      border = z$col[k]
    )
  }

  # Add axis
  x <- seq(from = xinit, to = xinit + .17 * xR, length.out = n)
  graphics::lines(x = c(xinit, xinit + .17 * xR), y = rep(z$y2[1], 2))
  for (i in 1:n) graphics::lines(x = rep(x[i], 2), y = c(z$y2[1], z$y2[1] - .003 * yR))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 0 values as transparent
  g0 <- .010 * xR
  g1 <- .040 * xR
  nz <- nrow(z)
  graphics::polygon(
    x = c(z$x2[nz] + g0, z$x2[nz] + g1, z$x2[nz] + g1, z$x2[nz] + g0, z$x2[nz] + g0),
    y = c(z$y1[k], z$y1[k], z$y2[k], z$y2[k], z$y1[k]),
    col = "#00000000",
    border = "#000000",
    lwd = .5
  )
  graphics::text(
    labels = "0",
    x = mean(c(z$x2[nz] + g0, z$x2[nz] + g1)),
    y = mean(c(z$y1, z$y2)), adj = c(.5, .5),
    cex = cexSub * .75
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Labels
  if (range[2] <= 5) {
    lab <- round(seq(from = 0, to = max(range[2]), length.out = n), 2)
  } else {
    lab <- round(seq(from = 0, to = max(range[2]), length.out = n))
  }

  # Add that it's > 0, not 0
  lab[1] <- glue::glue("> {lab[1]}")

  graphics::text(
    x = x,
    y = rep(z$y2[1] - .01 * yR, n),
    labels = lab,
    cex = cexSub * .75,
    adj = c(1, 1),
    srt = 45
  )

  # Add titles
  yText <- ybarUp + .035 * yR

  # Add sub text
  if (!is.null(subTitle)) {
    graphics::text(
      x = xinit,
      y = yText,
      labels = latex2exp::TeX(subTitle, italic = TRUE),
      cex = cexSub,
      adj = c(0, 1)
    )
    yText <- yText + .0224 * yR
  }

  # Add main title
  if (!is.null(mainTitle)) {
    graphics::text(
      x = xinit,
      y = yText,
      labels = mainTitle,
      cex = cexMain,
      font = 2,
      adj = c(0, 1)
    )
  }
}


# =================================================================
#' @rdname plot_legend
#' @export
plot_legend_bin <- function(col,
                            cexMain = 1,
                            cexSub = .75,
                            minUp = .175,
                            mainTitle = NULL,
                            subTitle = NULL) {

  # Determine plot boundaries, in units of the data
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  xR <- xmax - xmin
  yR <- ymax - ymin

  xinit <- xmin + .05 * xR # minimum left side to write
  yinit <- ymax - minUp * yR # minimum upper side to write
  ygap <- .15 * yR
  xgap <- .014 * xR
  ybarUp <- yinit - ygap / 2 - .0041 * yR
  ybarDn <- yinit - ygap - ygap / 2 + .0041 * yR


  # Plot
  sq <- .05
  polygon(
    x = c(xinit, xinit, xinit + sq * xR, xinit + sq * xR, xinit),
    y = c(ybarUp, ybarDn, ybarDn, ybarUp, ybarUp),
    col = col,
    border = "#000000"
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add titles
  yText <- ybarUp + .035 * yR

  # Add sub text
  if (!is.null(subTitle)) {
    graphics::text(
      x = xinit,
      y = yText,
      labels = latex2exp::TeX(subTitle, italic = TRUE),
      cex = cexSub,
      adj = c(0, 1)
    )
    yText <- yText + .0224 * yR
  }

  # Add main title
  if (!is.null(mainTitle)) {
    graphics::text(
      x = xinit,
      y = yText,
      labels = mainTitle,
      cex = cexMain,
      font = 2,
      adj = c(0, 1)
    )
  }
}
