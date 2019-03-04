# acceleration_x()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
# Emiliano Zaccarella, zaccarella@cbs.mpg.de
#
# Compute acceleration for points on y-axis for given data frame in OpenPoseR format

acceleration_y <- function(data, model, fps = 25) {
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

  # Warn user if fps wasn't specified
  if(missing(fps)) {
    warning("You didn't specify the original video files frames per second (fps). Defaulting to 25.",
         call. = FALSE)
  }

  # Compute duration of one frame: 1 sec / fps
  k <- 1/fps

  # Determine number of points to use (depending on model argument)
  if(model=="body25") {
    # List of points in model (BODY25)
    points_y <- paste("y", 0:24, sep = "")
    num_cols <- 25
  } else if(model=="hands") {
    # List of points in model (hands)
    points_y <- paste("y", 0:20, sep = "")
    num_cols <- 21
  } else if(model=="face") {
    # List of points in model (face)
    points_y <- paste("y", 0:69, sep = "")
    num_cols <- 70
  }

  # Create empty data frames
  acceleration_y <- data.frame(matrix(nrow = 0, ncol = num_cols))
  acceleration_y <- setNames(acceleration_y, points_y)

  # Compute acceleration (y-axis) for all points in model
  for(i in 1:(nrow(data)-1)){
    # Formula: (((pixel at t1)-(pixel at t1-1))/((t1)-(t1-1)))/((t1)-(t1-1))
    acceleration_y <- rbind(acceleration_y, (data[i+1,points_y] -
                                               data[i,points_y])/k)
  }

  # Return result
  return(acceleration_y)
}
