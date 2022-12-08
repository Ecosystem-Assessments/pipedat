# ------------------------------------------------------------------------------
# List of parameters for `dp_{uid}-name()` and 'pipedat()' functions
# NOTE: Documented here in order to avoid unnecessary repetition in documentation
dp_params <- function() {
  c(
    "@param bbox bounding box to spatially subset the queried data, if applicable. The bounding box should be of the form `c(xmin, ymin, xmax, ymax)`",
    "@param bbox_crs spatial projection of bounding box argument, defaults to epsg: 4326",
    "@param timespan time span to temporally subset the queried data, if applicable. The time span should a vector containing all the years to be queried `c(year1, year2, ...)`"
  )
}

# ------------------------------------------------------------------------------
# List of parameters for `di_{uid}-name()` and 'pipein()' functions
# NOTE: Documented here in order to avoid unnecessary repetition in documentation
di_params <- function() {
  c(
    "@param grid spatial grid used for data integration. Can be a `sf` object containing polygons or a `stars` rasters that will be used as a template"
  )
}

grid_params <- function() {
  c(
    "@param aoi object of class sf or sfc representing the area of interest, or the bounding box of the area to consider. The bounding box should be either a `bbox` class object from the `sf` package or of the form `c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)`",
    "@param cellsize target cellsize",
    "@param crs object of class crs; coordinate reference system of the target of the target grid in case argument x is missing, if x is not missing, its crs is inherited."
  )
}
