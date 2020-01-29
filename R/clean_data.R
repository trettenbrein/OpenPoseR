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
    for(point in 0:24) {
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
    # We do not need to continue if nothing was tracked, i.e. all data is 0
    if(!all(data_points[c]==0)) {
      for(r in 2:nrow(data_points)-1) {
        # Check what to do with 0 values
        ## There is some redundancy here -> Maybe this can be streamlined to be
        ## only one statement?
        if(data_points[r,c]==0 && data_points[r-1,c]!=0 &&
           data_points[r+1,c]!=0) {
          # If point wasn't dedected (position = 0), compute mean from +-1 frame
          # But only, if these values aren't zero either!
          # && data_points[r-1,c]!=0 && data_points[r+1,c]!=0
          data_points[r,c] <- (data_points[r-1,c]+data_points[r+1,c])/2
        } else if(data_points[r,c]==0 && data_points[r-1,c]!=0) {
          #
          # Last non-zero value should alwys be row above (-1)
          last_non_zero <- -1

          # Get next non-zero value
          next_non_zero <- 1
          while(r+next_non_zero<=nrow(data_points) &&
             data_points[r+next_non_zero,c]==0) {
            # Try next row by increasing counter
            next_non_zero <- next_non_zero+1
          }

          # If we're at the end of the dataframe, just use the previous value
          # In effect, the row will then be set to the value of the row above
          if(next_non_zero==nrow(data_points)) {
            next_non_zero <- -1
          }

          # Compute mean and save it
          data_points[r,c] <- (data_points[r+last_non_zero,c]+
                                 data_points[r+next_non_zero,c])/2
        }
      }
    }
  }

  # Return result
  data[,points] <- data_points
  return(data)
}
