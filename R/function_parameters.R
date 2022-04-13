# ------------------------------------------------------------------------------
# List of parameters for `dp_{uid}-name()` and 'pipedat()' functions
# NOTE: Documented here in order to avoid unnecessary repetition in documentation
doc_params <- function() {
  c(
    "@param output output folder for queried data. That folder will be organized into the different files that are loaded, i.e. raw data, formatted data, metadata and bibtex files. By default, data are stored in a root folder called 'data/'",
    "@param input input folder in case data has to be loaded from disk, e.g. for datasets that are bound by a data sharing agreement. By default, raw data are searched for in a folder called 'data/data-raw/'",
    "@param bbox bounding box to spatially subset the queried data, if applicable. The bounding box should be of the form `c(xmin, ymin, xmax, ymax)`",
    "@param timespan time span to spatially subset the queried data, if applicable. The time span should be of the form `c(time_from, time_to)`"
  )
}

# ------------------------------------------------------------------------------
# Series of functions for messages
# Message for the need to have the raw data

# This function requires the raw data in folder './'
