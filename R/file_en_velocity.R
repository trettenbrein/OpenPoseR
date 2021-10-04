# file_en_velocity()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# A wrapper for the OpenPoseR function en_velocity() that makes it possible to
# directly pass two file names and or paths including a file name to the function.

file_en_velocity <- function(file1, file2, start_from_zero = TRUE) {
  # We need file 1 (x)
  if(missing(file1)) {
    stop("Argument \"file1\" must be specified. Path to CSV file inclduing file name and ending (*.csv).", call. = FALSE)
  }

  # We need file 2 (y)
  if(missing(file2)) {
    stop("Argument \"file2\" must be specified. Path to CSV file inclduing file name and ending (*.csv).", call. = FALSE)
  }

  # Check whether files exists
  if(!file.exists(file1)) {
    stop(paste("Couldn't find CSV file: ", file1, sep = ""), call. = FALSE)
  }
  if(!file.exists(file2)) {
    stop(paste("Couldn't find CSV file: ", file2, sep = ""), call. = FALSE)
  }

  # Read files
  data1 <- read.csv(file = file1)
  data2 <- read.csv(file = file2)

  # Call en_velocity()
  en <- en_velocity(data1, data2, start_from_zero = start_from_zero)

  # Update file (i.e. overwrite) or crate new file?
  output_file <- try(write.csv(en, file = paste(gsub("\\_velocity_x.csv$", "", file1),
                                                "_en_velocity.csv", sep = ""),
                               row.names = FALSE))

  # We should tell the user whether the operation was a success
  output <- FALSE
  ## See if files were created successfully, if not issue a warning
  if(is.null(output_file)) {
    output <- TRUE
  } else {
    warning("Creating file for results of calling file_en_acceleration() failed.",
            call. = FALSE)
  }

  # Return message about result
  return(output)
}
