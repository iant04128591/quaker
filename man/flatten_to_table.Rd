% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flatten_to_table.R
\name{flatten_to_table}
\alias{flatten_to_table}
\title{flatten geojson object to data table with must salient covariates}
\usage{
flatten_to_table(seismic.geojson.obj)
}
\arguments{
\item{seismic.geojson.obj}{GeoJson object}
}
\value{
A flattened dataset to tabular format, useful for analysis.
}
\description{
flatten geojson object to data table with must salient covariates
}
\examples{
data <- flatten_to_table(get_seismic_data(timeFrame = 'PAST_DAY', minMagnitude = 'all'))
data
data <- flatten_to_table(get_seismic_data(timeFrame = 'PAST_WEEK', minMagnitude = '1'))
data
data <- flatten_to_table(get_seismic_data(timeFrame = 'PAST_MONTH', minMagnitude = '2.5'))
data
}
\seealso{
\code{\link{get_seismic_data}},
 \code{\link{fit_quaker}},
 \code{\link{plot}},
 \code{\link{get_freq_grp_by_country_mag}},
 \code{\link{get_freq_grp_by_day_mag}}
}

