#' Plot geojson object
#'
#' @param x GeoJson Object
#' @param ... other arguments to be handle internally by the plot function
#'
#' @return Plots an object of type seismic_geojson.
#' @export
#'
#' @importFrom dplyr "%>%" select
#' @importFrom tidyr spread
#' @importFrom rCharts Leaflet
#'
#' @seealso
#'  \code{\link{get_seismic_data}},
#'  \code{\link{fit_quaker}},
#'  \code{\link{flatten_to_table}},
#'  \code{\link{get_freq_grp_by_country_mag}},
#'  \code{\link{get_freq_grp_by_day_mag}}
#'
#' @examples
#' plot(get_seismic_data(timeFrame = 'PAST_WEEK', minMagnitude = '1'))
#' plot(get_seismic_data(timeFrame = 'PAST_MONTH', minMagnitude = '2.5'))
#'
#'
#'
plot.seismic_geojson = function(x, ...){

  my_map = Leaflet$new()
  my_map$tileLayer(provider='Esri.WorldStreetMap')
  my_map$setView(c(0, 0), zoom = 2)
  for(i in 1:nrow(x$features)) {

    longtitude <-  x$features[i,]$geometry$coordinates[[1]][1]
    latitude <-  x$features[i,]$geometry$coordinates[[1]][2]
    place <- x$features[i,]$properties$place
    magnitude <- x$features[i,]$properties$mag
    time <- as.POSIXct(x$features[i,]$properties$time/1000, origin="1970-01-01")
    depth <-  x$features[i,]$geometry$coordinates[[1]][3]
    url <- x$features[i,]$properties$url

        my_map$marker(c(latitude, longtitude),
                  bindPopup = paste(
                    "<ul>",
                    "<li>Place:",place,"</li>",
                    "<li>Magnitude:",magnitude,"</li>",
                    "<li>Time:",time,"</li>",
                    "<li>Depth:",depth," km</li>",
                    "<li><a href=",url,">More Details...</></li>",
                    "</ul>"))
  }
  my_map

}

