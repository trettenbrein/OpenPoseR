\name{plot_timeseries}
\alias{plot_timeseries}
\title{Plot a time series (velocity or accelleration), derived from OpenPose data using OpenPoseR.}
\usage{
plot_timeseries(data, dataType)
}
\arguments{
\item{data}{Data frame containing time series to be plotted.}
\item{dataType}{Optional. Defines what data is being plot. 1 = "Euclidean norm of sums of velocity vectors."" 2 = "Euclidean norm of sums of accelleration vectors." Defaults to 1. Can be overwriten (to customize the y-axis label) by passing any string to the function.}
\item{legend}{Logical. Should the a legend be included in the plot? Defaults to FALSE. Notice, however, that the legend will not be plot by default if the data frame only contains data for one video clip (i.e. one column). This behaviour can be overwritten by specifiyng legend = TRUE.}
}
\description{
Creates a simple plot (using ggplot2) of time series data dervied by, for example, using en_velocity().

The data frame passed to this function must contain at least one column of time series data. If this data frame contains more than one column, every column will be plotted as individual time series in the same plot and a legend will be added. If the data frame does not contain a column labelled "FrameNr" then it will be assumed that the order of rows equals the order and number of frames.
}
\examples{
plot_timeseries(data)

plot_timeseries(data, "2")

plot_timeseries(data, "Some other unit for this time series")
}
