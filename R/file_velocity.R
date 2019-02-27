# file_velocity()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# A wrapper for the OpenPoseR functions velocity_x() and velocity_y() that makes it
# possible to directly pass a file name and or path including a file name to the
# function.

file_velocity <- function(file, model, fps = 25) {
  # We at least need an input file
  if(missing(file)) {
    stop("Argument \"file\" must be specified. Path to CSV file inclduing file name and ending (*.csv).", call. = FALSE)
  }

  # Check whether file exists
  if(!file.exists(file)) {
    stop(paste("Couldn't find CSV file: ", file, sep = ""), call. = FALSE)
  }

  # Check whether file name contains information about model
  # If file was created by create_csv() then it should
  if(missing(model)) {
    if(as.logical(grep("_body25", file))==TRUE) {
      model <- "body25"
    } else if(as.logical(grep("_hand_left", file))==TRUE | as.logical(
      grep("_hand_right", file))==TRUE ) {
      model <- "hands"
    } else if(as.logical(grep("_face", file))==TRUE) {
      model <- "face"
    } else {
      stop("Argument \"model\" must be specified. Tell me from which model the data came from (body25, hands, or face).", call. = FALSE)
    }
  }

  # Read file
  data <- read.csv(file = file)

  # Call velocity_x() and velocity_y()
  velocityX <- velocity_x(data, model, fps = fps)
  velocityY <- velocity_y(data, model, fps = fps)

  # Write results to files
  file1 <- try(write.csv(velocityX, file = paste(gsub("\\.csv$", "", file),
                                                 "_velocity_x.csv", sep = ""),
                         row.names = FALSE))
  file2 <- try(write.csv(velocityY, file = paste(gsub("\\.csv$", "", file),
                                                 "_velocity_y.csv", sep = ""),
                         row.names = FALSE))

  # We'll have to tell the user whether calling the operation was a success
  output <- FALSE
  ## See if files were created successfully, if not issue warnings
  if(is.null(file1) && is.null(file2)) {
    output <- TRUE
  } else if(!is.null(file1)) {
    warning("Creating file for results of calling velocity_x() failed.",
            call. = FALSE)
  } else if(!is.null(file2)) {
    warning("Creating file for results of calling velocity_y() failed.",
            call. = FALSE)
  }

  # Return message about result
  return(output)
}
