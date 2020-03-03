\name{plot_frontal}
\alias{plot_frontal}
\title{Plot a heatmap of points extracted from OpenPose data using create_csv().}
\usage{
plot_frontal(plotData, width = 720, height = 576, backgroundImage = NULL, quadrantLabels = TRUE, quadrantLabelSize = 12, binSize = 125)
}
\arguments{
\item{plotData}{Data frame containing x and y coordinates of tracked points in OpenPoseR format, that is as generated from OpenPose data by create_csv().}
\item{width}{Width of video from which data will be plotted. Defaults to 720 pixels.}
\item{height}{height of video from which data will be plotted. Defaults to 576 pixels.}
\item{backgroundImage}{Pass an alternative packground image to the function, if the default one is not suitable for your purpose. (Read images using magick::image_read() function.)}
\item{quadrantLabels}{Display quadrant labels. Defaults to TRUE.}
\item{quadrantLabelSize}{Change size of quadrant labels. Default is 12.}
\item{binSize}{Determine size of bins in plot. Default is 125.}
}
\description{
Creates a simple heatmap (using ggplot2) of points in a body-pose model (x,y).
}
\examples{
plot_frontal(data)
}
