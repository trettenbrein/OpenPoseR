\name{file_video_index}
\alias{file_video_index}
\title{Create video index using video_index() for a given directory and save it as a CSV file.}
\usage{
file_video_index(videoPath, outputPath, fileType, recursive)
}
\arguments{
\item{videoPath}{Path to the directory containing the video files to be indexed.}
\item{outputPath}{Optional. Path to the directory where the outptu file should be saved. By default, the index file ("video_index.csv") will be saved in the specified input directory.}
\item{fileType}{Optional. What video file type should be considered for indexing? Defaults to ".mp4".}
\item{recursive}{Optional. Should video files in sub-directories be considered? If yes, set to TRUE. Defaults to FALSE.}
}
\description{
A wrapper for the OpenPoseR function video_index() that makes it possible to directly pass a directory name (i.e. path) to the function.

Can be used to use video_index() for a directory of video files and automatically saving the index to the specified input directory without manually storing and saving the video index data frame created by the OpenPoseR function.
}
\examples{
file_video_index("/home/user/my_stimulus_clips")

file_video_index("/home/user/my_stimulus_clips")", fileType = ".avi", recursive = TRUE)
}
