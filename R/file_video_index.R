# file_video_index()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# A wrapper for the OpenPoseR functions velocity_x() and velocity_y() that makes it
# possible to directly pass a file name and or path including a file name to the
# function.

file_video_index <- function(videoPath, outputPath = videoPath, fileType = ".mp4", recursive = FALSE) {
  # We at least need an input directory
  if(missing(videoPath)) {
    stop("Argument \"videoPath\" must be specified. Path to the input directory containing the video files", call. = FALSE)
  }

  # Check whether directory exists
  if(!file.exists(videoPath)) {
    stop(paste("Couldn't find directory: ", videoPath, sep = ""), call. = FALSE)
  }

  # Create index
  videoIndex <- video_index(videoPath, fileType = fileType,
                            recursive = recursive)

  # Write results to files
  file <- try(write.csv(videoIndex, file = paste(outputPath, "video_index.csv",
                                                 sep = ""), row.names = FALSE))

  # We'll have to tell the user whether calling the operation was a success
  output <- FALSE
  ## See if files were created successfully, if not issue warnings
  if(is.null(file)) {
    output <- TRUE
  } else if(!is.null(file)) {
    warning("Creating file for video index failed.",
            call. = FALSE)
  }

  # Return message about result
  return(output)
}
