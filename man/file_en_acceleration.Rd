\name{file_en_acceleration}
\alias{file_en_acceleration}
\title{Compute en_acceleration() for given CSV files generated using file_acceleration()}
\usage{
file_en_acceleration(file1, file2)
}
\arguments{
\item{file1}{Name of first CSV file (*.csv) or path to file including filename. This file should contain the acceleration for the x-axis, computed using acceleration_x().}
\item{file2}{Name of second CSV file (*.csv) or path to file including filename. This file should contain the acceleration for the y-axis, computed using acceleration_y().}
}
\description{
Compute en_acceleration() for given CSV files generated using file_acceleration().
}
\examples{
# Run en_acceleration() for files "~/myvideo/myvideo_body25_acceleration_x.csv"
# and "~/myvideo/myvideo_body25_acceleration_y.csv"
file_en_acceleration("~/myvideo/myvideo_body25_acceleration_x.csv",
"~/myvideo/myvideo_body25_acceleration_y.csv")
}
