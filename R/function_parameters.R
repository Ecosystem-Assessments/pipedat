# ------------------------------------------------------------------------------
# List of parameters for `dp_{uid}-name()` and 'pipedat()' functions
# NOTE: Documented here in order to avoid unnecessary repetition in documentation
doc_params <- function() {
  c(
    "@param output output folder for queried data. That folder will be organized into the different files that are loaded, i.e. raw data, formatted data, metadata and bibtex files. By default, data are stored in a root folder called 'data/'",
    "@param name name of the dataset to use for storing purposes. A default value is provided in individual data pipelines, but it can be modified by the user, for example by a numeric and sequential identifier for a specific project.",
    "@param crs spatial projection to use to transform the spatial data into a uniform projection",
    "@param bbox bounding box to spatially subset the queried data, if applicable. The bounding box should be of the form `c(xmin, ymin, xmax, ymax)`",
    "@param timespan time span to temporally subset the queried data, if applicable. The time span should a vector containing all the years to be queried `c(year1, year2, ...)`",
    "@param ... further arguments used in individual data pipelines, if applicable."
  )
}