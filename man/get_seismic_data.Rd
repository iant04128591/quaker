% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_seismic_data.R
\name{get_seismic_data}
\alias{get_seismic_data}
\title{Get geojson object from USGS}
\usage{
get_seismic_data(timeFrame = c("PAST_HOUR", "PAST_DAY", "PAST_WEEK",
  "PAST_MONTH"), minMagnitude = c("all", "1.0", "2.5", "4.5"))
}
\arguments{
\item{timeFrame}{One of: PAST_HOUR, PAST_DAY, PAST_WEEK, PAST_MONTH}

\item{minMagnitude}{One of: 'all','1.0','2.5','4.5'}
}
\value{
A GeoJson object
}
\description{
Get geojson object from USGS
}
\examples{

data <- get_seismic_data(timeFrame = 'PAST_WEEK', minMagnitude = '1')
str(data)
data <- get_seismic_data(timeFrame = 'PAST_MONTH', minMagnitude = '2.5')
str(data)
}
\seealso{
\code{\link{fit_quaker}},
 \code{\link{flatten_to_table}},
 \code{\link{get_freq_grp_by_country_mag}},
 \code{\link{get_freq_grp_by_day_mag}},
 \code{\link{plot}}
}

