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
      for(r in 1:nrow(data_points)-1) {
        # If a point wasn't dedected (position = 0), compute mean from +-1 frame
        # But only, if these values aren't zero either!
        # && data_points[r-1,c]!=0 && data_points[r+1,c]!=0
        if(data_points[r,c]==0 && data_points[r-1,c]!=0 && data_points[r+1,c]!=0) {
          data_points[r,c] <- (data_points[r-1,c]+data_points[r+1,c])/2
        }
      }
    }
  }

  # Return result
  data[,points] <- data_points
  return(data)
}
