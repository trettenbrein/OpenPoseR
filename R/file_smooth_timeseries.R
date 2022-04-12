# file_smooth_timeseries()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# A wrapper for the OpenPoseR function smooth_timeseries() that makes it possible to
# directly pass a file name and or path including a file name to the function.

file_smooth_timeseries <- function(file, span = 4, order = 6, overwrite = FALSE) {
  # We at least need an input file
  if(missing(file)) {
    stop("Argument \"file\" must be specified. Path to CSV file inclduing file name and ending (*.csv).", call. = FALSE)
  }

  # Check whether file exists
  if(!file.exists(file)) {
    stop(paste("Couldn't find CSV file: ", file, sep = ""), call. = FALSE)
  }

  # Read file
  data <- read.csv(file = file)

  # Call clean_data()
  smoothed_data <- smooth_timeseries(data[,1], span, order)

  # Update file (i.e. overwrite) or crate new file?
  if(overwrite==FALSE) {
    output_file <- try(write.csv(smoothed_data, file = paste(gsub("\\.csv$", "", file),
                                                           "_smoothed.csv", sep = ""),
                                 row.names = FALSE))
  } else {
    output_file <- try(write.csv(smoothed_data, file = file, row.names = FALSE))
  }

  # We should tell the user whether the operation was a success
  output <- FALSE
  ## See if files were created successfully, if not issue a warning
  if(is.null(output_file)) {
    output <- TRUE
  } else {
    warning("Creating file for results of calling smooth_timeseries() failed.",
            call. = FALSE)
  }

  # Return message about result
  return(output)
}
