% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeflow.R
\name{pipeflow}
\alias{pipeflow}
\title{Execute a complete data workflow}
\usage{
pipeflow(config = "./project-data/pipedat/pipeflow.yml")
}
\arguments{
\item{config}{path to a yaml data workflow configuration file prepared by the user. Use \code{pipenew()} to generate a new configuration file template. Alternatively, this can be a list organized as a yaml document.}
}
\value{
This function returns the queried raw data, formatted data, metadata and bibtex associated with the raw data, and all integrated data.
}
\description{
This function is used execute an entire data workflow that will download, format, integrate, and incorporate all spatial data into a regular grid to generate a ready to work with integrated dataset. It uses a data workflow yaml configuration file to execute the complete the workflow.
}
\examples{
\dontrun{
pipeflow(config = "./project-data/config/pipedat.yml")
}
}
