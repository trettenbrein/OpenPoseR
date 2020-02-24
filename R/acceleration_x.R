# acceleration_x()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
# Emiliano Zaccarella, zaccarella@cbs.mpg.de
#
# Compute acceleration for points on x-axis for given data frame in OpenPoseR format

acceleration_x <- function(data, model, fps = 25) {
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
    points_x <- paste("x", 0:24, sep = "")
    num_cols <- 25
  } else if(model=="hands") {
    # List of points in model (hands)
    points_x <- paste("x", 0:20, sep = "")
    num_cols <- 21
  } else if(model=="face") {
    # List of points in model (face)
    points_x <- paste("x", 0:69, sep = "")
    num_cols <- 70
  }

  # Create empty data frames
  acceleration_x <- data.frame(matrix(nrow = 0, ncol = num_cols))
  acceleration_x <- setNames(acceleration_x, points_x)

  # Compute acceleration (x-axis) for all points in model
  for(i in 1:(nrow(data)-2)){
    # Formula: ((((point at t1)-(point at t1-1))/((t1)-(t1-1)))-
    # (((point at t1)-(point at t1-1))/((t1)-(t1-1))))/((t1)-(t1-1)))
    acceleration_x <- rbind(acceleration_x, (((data[i+2,points_x] -
                                               data[i+1,points_x])/k) -
                                               ((data[i+1,points_x] -
                                               data[i,points_x])/k))/k)
  }

  # Return result
  return(acceleration_x)
}
