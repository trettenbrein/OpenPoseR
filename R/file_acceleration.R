# file_acceleration()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# A wrapper for the OpenPoseR functions acceleration_x() and acceleration_y()
# that makes it possible to directly pass a file name and or path including a
# file name to the function.

file_acceleration <- function(file, model, fps = 25) {
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
    if(grepl("_body25", file)==TRUE) {
      model <- "body25"
    } else if(grepl("_hand_left", file)==TRUE | grepl("_hand_right", file)==TRUE ) {
      model <- "hands"
    } else if(grepl("_face", file)==TRUE) {
      model <- "face"
    } else {
      stop("Argument \"model\" must be specified. Tell me from which model the data came from (body25, hands, or face).", call. = FALSE)
    }
  }

  # Read file
  data <- read.csv(file = file)

  # Call acceleration_x() and acceleration_y()
  accelerationX <- acceleration_x(data, model, fps = fps)
  accelerationY <- acceleration_y(data, model, fps = fps)

  # Write results to files
  file1 <- try(write.csv(accelerationX, file = paste(gsub("\\.csv$", "", file),
                                               "_acceleration_x.csv", sep = ""),
                         row.names = FALSE))
  file2 <- try(write.csv(accelerationY, file = paste(gsub("\\.csv$", "", file),
                                               "_acceleration_y.csv", sep = ""),
                         row.names = FALSE))

  # We'll have to tell the user whether calling the operation was a success
  output <- FALSE
  ## See if files were created successfully, if not issue warnings
  if(is.null(file1) && is.null(file2)) {
    output <- TRUE
  } else if(!is.null(file1)) {
    warning("Creating file for results of calling acceleration_x() failed.",
            call. = FALSE)
  } else if(!is.null(file2)) {
    warning("Creating file for results of calling acceleration_y() failed.",
            call. = FALSE)
  }

  # Return message about result
  return(output)
}
