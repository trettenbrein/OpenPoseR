# file_clean()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# A wrapper for the OpenPoseR function clean_data() that makes it possible to
# directly pass a file name and or path including a file name to the function.

file_clean <- function(file, model, cutoff = 0.1, overwrite = TRUE) {
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

  # Call clean_data()
  claned_data <- clean_data(data, model, cutoff)

  # Update file (i.e. overwrite) or crate new file?
  if(overwrite==FALSE) {
    output_file <- try(write.csv(claned_data, file = paste(gsub("\\.csv$", "", file),
                                                           "_cleaned.csv", sep = ""),
                                 row.names = FALSE))
  } else {
    output_file <- try(write.csv(claned_data, file = file, row.names = FALSE))
  }

  # We should tell the user whether the operation was a success
  output <- FALSE
  ## See if files were created successfully, if not issue a warning
  if(is.null(output_file)) {
    output <- TRUE
  } else {
    warning("Creating file for results of calling clean_data() failed.",
            call. = FALSE)
  }

  # Return message about result
  return(output)
}
