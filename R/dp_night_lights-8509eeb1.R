#' @eval get_name("8509eeb1")
#'
#' @eval get_description("8509eeb1")
#'
#' @eval doc_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 8509eeb1
#'
#' @examples
#' \dontrun{
#' dp_8509eeb1()
#' }
dp_8509eeb1 <- function(output, crs = 4326, bbox = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "8509eeb1"
  name <- get_shortname(uid)
  nm <- glue("{name}-{uid}")
  output <- make_output(uid, name, output, local = TRUE)
  path <- glue("{output}{nm}/")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # If the data is downloaded from online sources.
  # In this particular case, access to the data requires a user account. 
  # The data can be downloaded at the following url: 
  # https://eogdata.mines.edu/nighttime_light/annual/v20/
  #
  # More specifically, the following urls lead to the specific datasets that were 
  # used for this data pipeline:  
  # urls <- c(
  #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2012/VNL_v2_npp_201204-201303_global_vcmcfg_c202102150000.average_masked.tif.gz",
  #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2013/VNL_v2_npp_2013_global_vcmcfg_c202102150000.average_masked.tif.gz",
  #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2014/VNL_v2_npp_2014_global_vcmslcfg_c202102150000.average_masked.tif.gz",
  #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2015/VNL_v2_npp_2015_global_vcmslcfg_c202102150000.average_masked.tif.gz",
  #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2016/VNL_v2_npp_2016_global_vcmslcfg_c202102150000.average_masked.tif.gz",
  #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2017/VNL_v2_npp_2017_global_vcmslcfg_c202102150000.average_masked.tif.gz",
  #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2018/VNL_v2_npp_2018_global_vcmslcfg_c202102150000.average_masked.tif.gz",
  #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2019/VNL_v2_npp_2019_global_vcmslcfg_c202102150000.average_masked.tif.gz",
  #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2020/VNL_v2_npp_2020_global_vcmslcfg_c202102150000.average_masked.tif.gz",
  #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2021/VNL_v2_npp_2021_global_vcmslcfg_c202203152300.average_masked.tif.gz"
  # )
  # _________________________________________________________________________________________ #
    
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # IMPORT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  gzfiles <- dir(glue("{path}raw/"), pattern = ".gz", full.names = TRUE)
  lapply(gzfiles, function(x) R.utils::gunzip(x, remove = FALSE))
  # _________________________________________________________________________________________ #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # FORMAT DATA
  # NOTE: optional
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # CREATE METADATA
  # WARNING: mandatory
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- get_metadata(
    pipeline_id = uid,
    pipeline_crs = crs, 
    pipeline_bbox = bbox, 
    pipeline_timespan = timespan, 
    data_access = "2022-04-26", 
    data_bbox = c(xmin=-180,ymin=-90,xmax=180,ymax=90), 
    data_timespan = 2012:2021)
  )
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # CREATE BIBTEX
  # WARNING: mandatory
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  bib <- get_bib(uid)
  # _________________________________________________________________________________________ #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # APPLY SUBSETS AND CRS SPECIFIED BY USER
  # NOTE: optional, only if applicable
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  dat <- dp_parameters(
    dat, 
    crs = crs, 
    bbox = bbox, 
    timespan = timespan
  )
  # _________________________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # EXPORT 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Formatted data 
  fm <- glue("{path}/{nm}.geojson") # NOTE: not necessarily spatial data
  sf::st_write(dat, dsn = fm, quiet = TRUE) # for spatial data
  
  # Metadata
  mt <- glue("{path}/{nm}.yaml")
  yaml::write_yaml(meta, mt, column.major = FALSE)

  # Bibtex
  bi <- glue("{path}/{nm}.bib")
  RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
  # _________________________________________________________________________________________ #
}
