\name{file_en_velocity}
\alias{file_en_velocity}
\title{Compute en_velocity() for given CSV files generated using file_velocity()}
\usage{
file_en_velocity(file1, file2)
}
\arguments{
\item{file1}{Name of first CSV file (*.csv) or path to file including filename. This file should contain the velocity for the x-axis, computed using velocity_x().}
\item{file2}{Name of second CSV file (*.csv) or path to file including filename. This file should contain the velocity for the y-axis, computed using velocity_y().}
}
\description{
Compute en_velocity() for given CSV files generated using file_velocity().
}
\examples{
# Run en_velocity() for files "~/myvideo/myvideo_body25_velocity_x.csv"
# and "~/myvideo/myvideo_body25_velocity_y.csv"
file_en_velocity("~/myvideo/myvideo_body25_velocity_x.csv",
"~/myvideo/myvideo_body25_velocity_y.csv")
}
