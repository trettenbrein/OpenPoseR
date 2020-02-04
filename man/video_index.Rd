\name{video_index}
\alias{video_index}
\title{Create an index of video files and their properties (length, frame rate, etc.) in a given directory.}
\usage{
video_index(path, fileType, recursive)
}
\arguments{
\item{path}{Path to the directory containing the video files to be indexed.}
\item{fileType}{Optional. What video file type should be considered for indexing? Defaults to ".mp4".}
\item{recursive}{Optional. Should video files in sub-directories be considered? If yes, set to TRUE. Defaults to FALSE.}
}
\description{
Creates an index (i.e. a data frame) of the video files of a given filetype (e.g., ".mp4") in a given directory (path) and, optionally, its subdirectories (when recursive = TRUE). The function uses the R package "av" to read video files and creates an index with the following information for every file of the specified type found in the path: The file's name ("file_name"), file name including the full path to the file ("file_name_with_path"), the duration of the file in millisecond ("length_ms"), the width of the video in pixels ("width"), the height of the video in pixels ("height"), the video codec used ("codec"), the total number of frames in the video file ("frames"), the framerate of the video file ("framerate"), and the video format ("format").
}
\examples{
video_index("/home/user/my_stimulus_clips")

video_index("/home/user/my_stimulus_clips")", fileType = ".avi", recursive = TRUE)
}
