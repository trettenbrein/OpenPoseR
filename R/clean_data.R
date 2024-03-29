# clean_data()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# Description

clean_data <- function(data, model, cutoff = .1) {
  # We at least need some data to work with
  if(missing(data)) {
    stop("You have to pass the function a data frame to work with.",
         call. = FALSE)
  }

  # We need to know what data from what model we're working with
  if(missing(model)) {
    stop("Argument \"model\" must be specified. Tell me from which model the data came from (body25, hands, or face).",
         call. = FALSE)
  }

  # Check probability for every tracked point in data frame; remove if necessary
  for(row in 1:nrow(data)) {
    # Determine number of columns that we'll have to check depending on the model
    if(model=="body25") {
     columns <- 24
    } else if(model=="hands") {
      columns <- 20
    } else if(model=="face") {
      columns <- 69
    }

    for(point in 0:columns) {
      # Confidence in tracking smaller than or equal to cutoff?
      if(data[row,paste("c", point, sep = "")]<=cutoff) {
        # Set data points (x, y) and c to 0
        data[row,paste("x", point, sep = "")] <- 0
        data[row,paste("y", point, sep = "")] <- 0
        data[row,paste("c", point, sep = "")] <- 0
      }
    }
  }

  # Determine number of points to use (depending on model argument)
  if(model=="body25") {
    # List of points in model (BODY25)
    points <- c(paste("x", 0:24, sep = ""), paste("y", 0:24, sep = ""))
  } else if(model=="hands") {
    points <- c(paste("x", 0:20, sep = ""), paste("y", 0:20, sep = ""))
  } else if(model=="face") {
    points <- c(paste("x", 0:69, sep = ""), paste("y", 0:69, sep = ""))
  }

  # Check data quality: any zeros in there? how to handle them?
  data_points <- data[,points]
  # We have to check every row in every column
  for(c in 1:ncol(data_points)) {
    # We do not need to continue if nothing was tracked, i.e. all data is 0,
    # or if the first few values are 0 (i.e. point wasn't detected)
    # If points were not detected initially the whole column will be set to 0
    if(!all(data_points[c]==0) && data_points[1,c]!=0 && data_points[2,c]!=0 &&
       data_points[3,c]!=0) {
      for(r in 2:nrow(data_points)-1) {
        # Check what to do with 0 values
        if(!is.na(data_points[r,c]) && !is.na(data_points[r-1,c]) && data_points[r,c]==0 && data_points[r-1,c]!=0) {
          # If point wasn't detected (position = 0), compute mean from +-1 frame
          # If there is more than one consecutive non-zero points, use the next
          # tracked point instead and fill the gap with means. For the final
          # point (last row) in a data frame don't do this but use the value of
          # the last point that was tracked instead. -> i.e. this amounts to
          # imputing the missing data at the end of a data frame with the value
          # of the last detected point.

          # Last non-zero value should always be row above (-1)
          last_non_zero <- -1

          # Get next non-zero value
          next_non_zero <- 1
          while(r+next_non_zero<nrow(data_points) &&
             data_points[r+next_non_zero,c]==0) {
            # Try next row by increasing counter
            next_non_zero <- next_non_zero+1
          }

          # If we've reached the end of the data frame without any non-zero value
          # set point to 0. If not, compute mean and save it
          if(r+next_non_zero==nrow(data_points)) {
            data_points[r,c] <- 0
          } else {

            data_points[r,c] <- (data_points[r+last_non_zero,c]+
                                 data_points[r+next_non_zero,c])/2
          }
        }
      }
    } else {
      # Because the points were not detected initially, set them to 0
      data_points[c] <- 0
    }
  }

  # Return result
  data[,points] <- data_points
  return(data)
}
