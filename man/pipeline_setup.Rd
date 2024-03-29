% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{write_pipeline}
\alias{write_pipeline}
\alias{make_path}
\alias{check_raw}
\alias{check_format}
\alias{check_ingrid}
\alias{clean_path}
\alias{get_filepaths}
\alias{get_pipeline}
\alias{get_shortname}
\alias{get_name}
\alias{get_description}
\alias{get_contact}
\alias{get_creator}
\alias{get_citekey}
\alias{get_bib}
\alias{get_pipeline_url}
\alias{get_grid_info}
\title{Series of functions to process the data and integration pipelines}
\usage{
write_pipeline(uid)

make_path(uid)

check_raw(uid)

check_format(uid)

check_ingrid(uid)

clean_path(uid)

get_filepaths(uid)

get_pipeline(uid)

get_shortname(uid)

get_name(uid)

get_description(uid)

get_contact(uid)

get_creator(uid)

get_citekey(uid)

get_bib(uid)

get_pipeline_url(uid)

get_grid_info(grd = here::here("project-data", "grid", "grid.tif"))
}
\arguments{
\item{uid}{unique identifier of queried data.}
}
\description{
Series of functions to process the data and integration pipelines

Series of functions to process the data and integration pipelines

Series of functions to access metadata information
}
\section{Functions}{
\itemize{
\item \code{write_pipeline()}: base path for all data formatted through pipedat

\item \code{make_path()}: base path for all data formatted through pipedat

\item \code{check_raw()}: check if raw or formatted data exists

\item \code{check_format()}: check if formatted data exists

\item \code{check_ingrid()}: check if gridded data exists

\item \code{clean_path()}: create raw.zip if it does not exist and remove raw/

\item \code{get_filepaths()}: get path to files of a pipeline

\item \code{get_pipeline()}: get pipeline information

\item \code{get_shortname()}: get pipeline shortname

\item \code{get_name()}: get pipeline name

\item \code{get_description()}: get pipeline description

\item \code{get_contact()}: get pipeline contacts

\item \code{get_creator()}: get pipeline creator

\item \code{get_citekey()}: get pipeline bibtex citekey

\item \code{get_bib()}: get pipeline bibtex information

\item \code{get_pipeline_url()}: get pipeline url

\item \code{get_grid_info()}: get information on a grid

}}
