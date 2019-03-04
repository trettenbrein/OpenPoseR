\name{acceleration_y}
\alias{acceleration_y}
\title{Compute acceleration for points on y-axis for given data frame in OpenPoseR format}
\usage{
acceleration_y(data, model, fps = 25)
}
\arguments{
\item{data}{A data frame with separate clumns for every points x, y, and c values (OpenPoseR format).}
\item{model}{Specify which tracking model generated the data. Supply either body25, hands, or face.}
\item{fps}{Frames per second of the original video file. Defaults to 25.}
}
\description{
Compute acceleration for points on y-axis for given data frame in OpenPoseR format according to the following formula: (((pixel at t1)-(pixel at t1-1))/((t1)-(t1-1)))/((t1)-(t1-1))
}
\examples{
acceleration_y(x, "body25")
}
