# ------------------------------------------------------------------------------
# List of parameters for `dp_{uid}-name()` and 'pipedat()' functions
# NOTE: Documented here in order to avoid unnecessary repetition in documentation
dp_params <- function() {
  c(
    "@param output output folder for queried data. That folder will be organized into the different files that are loaded, i.e. raw data, formatted data, metadata and bibtex files. By default, data are stored in a root folder called 'data/'",
    "@param crs spatial projection to use to transform the spatial data into a uniform projection",
    "@param bbox bounding box to spatially subset the queried data, if applicable. The bounding box should be of the form `c(xmin, ymin, xmax, ymax)`",
    "@param timespan time span to temporally subset the queried data, if applicable. The time span should a vector containing all the years to be queried `c(year1, year2, ...)`",
    "@param ... further arguments used in individual data pipelines, if applicable."
  )
}

# ------------------------------------------------------------------------------
# List of parameters for `di_{uid}-name()` and 'pipein()' functions
# NOTE: Documented here in order to avoid unnecessary repetition in documentation
di_params <- function() {
  c(
    "@param output output folder for integrated data. That folder will be organized into the different files that are loaded, i.e. raw data, formatted data, metadata and bibtex files. By default, data are stored in a root folder called 'data/'",
    "@param grid spatial grid used for data integration. Can be a `sf` object containing polygons or a `stars` rasters that will be used as a template",
    "@param ... further arguments used in individual data integration pipelines, if applicable."
  )
}

grid_params <- function() {
  c(
    "@param x object of class sf or sfc",
    "@param bbox bounding box to spatially subset the queried data, if applicable. The bounding box should be of the form `c(xmin, ymin, xmax, ymax)`",
    "@param cellsize target cellsize",
    "@param crs object of class crs; coordinate reference system of the target of the target grid in case argument x is missing, if x is not missing, its crs is inherited."
  )
}