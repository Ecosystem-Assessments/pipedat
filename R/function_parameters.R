# ------------------------------------------------------------------------------
# List of parameters for `dp_{uid}-name()` and 'pipedat()' functions
# NOTE: Documented here in order to avoid unnecessary repetition in documentation and allow for easy modifications in the future if necessary.
dp_params <- function() {
  c(
    "@param bbox bounding box to spatially subset the queried data, if applicable. The bounding box should be of the form `c(xmin, ymin, xmax, ymax)`",
    "@param timespan time span to temporally subset the queried data, if applicable. The time span should a vector containing all the years to be queried `c(year1, year2, ...)`",
    "@param ingrid logical, whether data should be integrated in a study grid, if available for specific pipeline. Default is set to `TRUE`.",
    "@param keep_raw logical, whether raw data should be kept or removed. Default is set to `TRUE`. In any case, the raw data folder is compressed to save user disk space. Raw data can be accessed manually by unzipping `raw.zip`"
  )
}

grid_params <- function() {
  c(
    "@param aoi object of class sf or sfc representing the area of interest, or the bounding box of the area to consider. The bounding box should be either a `bbox` class object from the `sf` package or of the form `c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)`",
    "@param cellsize target cellsize",
    "@param crs object of class crs; coordinate reference system of the target of the target grid in case argument x is missing, if x is not missing, its crs is inherited."
  )
}
