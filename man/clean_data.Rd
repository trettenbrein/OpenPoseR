\name{clean_data}
\alias{clean_data}
\title{Remove points with low probability and points with zero values from a data frame}
\usage{
clean_data(data, model, cutoff)
}
\arguments{
\item{data}{A data frame with separate clumns for every points x, y, and c values.}
\item{model}{Specify which tracking model generated the data. Supply either body25, hands, or face.}
\item{cutoff}{Optional. Defaults to .1 as threshold and removes points (x,y) with confidence rating (c) below the cutoff from a frame.}
}
\description{
Removes points with zero values (i.e. where the tracking of a point failed completely) as well as points with extremely low confidence rating (default: .1) or a given confidence rating (cutoff) from the data frame provided.

By default, for points below the given confidence rating the frame is set to x,y = 0. In a second step, all points with zero values are replaced with mean values (x,y) derived from the previous and following frame.
}
\examples{
clean_data(x)
}
