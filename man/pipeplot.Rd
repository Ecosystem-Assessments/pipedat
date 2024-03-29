% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeplot.R
\name{pipeplot}
\alias{pipeplot}
\title{Generate figures of the pipedat project}
\usage{
pipeplot(res = 300, width = 225, height = 200, pal = viridis::viridis)
}
\value{
This function exports the figure of the area of interest in \verb{figures/pipedat/aoi/} and the gridded figures in \verb{figures/pipedat/ingrid/} in \code{png} format
}
\description{
This function creates a figure of the area of interest delineated by the study grid, and a figure for all data integrated in the study grid
}
\examples{
\dontrun{
# Generate report
pipeplot()
}
}
