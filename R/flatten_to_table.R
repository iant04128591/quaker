#' flatten geojson object to data table with must salient covariates
#'
#' @param seismic.geojson.obj GeoJson object
#'
#' @return A flattened dataset to tabular format, useful for analysis.
#' @export
#'
#' @seealso
#'  \code{\link{get_seismic_data}},
#'  \code{\link{fit_quaker}},
#'  \code{\link{plot.seismic_geojson}},
#'  \code{\link{get_freq_grp_by_country_mag}},
#'  \code{\link{get_freq_grp_by_day_mag}}
#'
#' @examples
#' data <- flatten_to_table(get_seismic_data(timeFrame = 'PAST_DAY', minMagnitude = 'all'))
#' data <- flatten_to_table(get_seismic_data(timeFrame = 'PAST_WEEK', minMagnitude = '1'))
#' data <- flatten_to_table(get_seismic_data(timeFrame = 'PAST_MONTH', minMagnitude = '2.5'))
#
flatten_to_table <- function(seismic.geojson.obj){

  data <- seismic.geojson.obj

  #get summary quantiles of 'longtiude','latitude','depth','magnitude'
  resp =  data$features %>% (function(x){
    mat <- do.call(rbind,data$features$geometry$coordinates)
    mat <- cbind(mat,data$features$properties$mag)
    mat <- cbind(mat,data$features$properties$tsunami)
    mat <- cbind(mat,data$features$properties$time)
    df <- as.data.frame(mat)
    colnames(df) = c('longtiude','latitude','depth','magnitude','tsunami','time')
    df
  })

  return(resp)
}
