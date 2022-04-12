\name{smooth_timeseries}
\alias{smooth_timeseries}
\title{Smooth time series data of given data frame using a Kolmogorov-Zurbenko filter.}
\usage{
smooth_timeseries(data, span = 4, order = 6)
}
\arguments{
\item{data}{A data frame with only one numerical column.}
\item{span}{The window for the filter.}
\item{order}{The number of iterations.}
}
\description{
This function can be used to smooth time series data of given data frame using a Kolmogorov-Zurbenko filter. By default applies a "mild" filter and imputes the tails.
}
\examples{
smooth_timeseries(data)
}
