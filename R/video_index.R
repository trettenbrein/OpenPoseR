# video_index()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# Returns an index of video files and their properties (framerate, etc.) when
# given a path.

video_index <- function(path, fileType = ".mp4", recursive = FALSE) {
  # We need a path to the folder we'll be using to look for files
  if(missing(path)) {
    stop("You have to pass the function a path so that we can index videos.",
         call. = FALSE)
  }

  # Check whether directory exists
  if(!file.exists(path)) {
    stop(paste("Couldn't find directory: ", path, sep = ""), call. = FALSE)
  }

  # Check wether a trailing slash was supplied; if not add it
  if(substr(path, nchar(path), nchar(path))!="/") {
    path <- paste(path, "/", sep = "")
  }

  # We'll be using "av" for this, so let's require it
  if (!requireNamespace("av", quietly = TRUE)) {
    stop("Package \"av\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else {
    # Load "av" package
    library("av")
  }

  # Get list of video file names in the given directory
  if(recursive==TRUE){
    files <- list.files(path, pattern = fileType, recursive = TRUE)
  } else {
    files <- list.files(path, pattern = fileType)
  }

  # Create data frame for infos read from files
  videoIndex <- data.frame(matrix(ncol = 9))
  colnames(videoIndex) <- c("file_name", "file_name_with_path", "length_ms", "width", "height", "codec", "frames", "framerate", "format")

  # Get video data for every file of given type in directory
  for(f in 1:length(files)){
    # Store file name
    videoIndex[f,"file_name"] <- files[f]

    # Get full path and also save it
    fullPath <- paste(path, files[f], sep = "")
    videoIndex[f,"file_name_with_path"] <- fullPath

    # Get video info and save it
    videoData <- av_media_info(fullPath)

    videoIndex[f,"length_ms"] <- videoData$duration*1000
    videoIndex[f,"width"] <- videoData$video[[1]]
    videoIndex[f,"height"] <- videoData$video[[2]]
    videoIndex[f,"codec"] <- videoData$video[[3]]
    videoIndex[f,"frames"] <- videoData$video[[4]]
    videoIndex[f,"framerate"] <- videoData$video[[5]]
    videoIndex[f,"format"] <- videoData$video[[6]]
  }

  # Return result
  return(videoIndex)
}
