# en_acceleration()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
# Emiliano Zaccarella, zaccarella@cbs.mpg.de
#
# Compute Euclidean norm of sums of acceleration vectors for a tracked video
# using the x-axis and y-axis data generated using acceleration_x() and
# acceleration_y().

en_acceleration <- function(acc_x, acc_y) {
  # We at least need some data to work with
  if(missing(acc_x) | missing(acc_y)) {
    stop("You have to pass the function two data frames (acceleration on x-axis and y-axis) to work with.",
         call. = FALSE)
  }

  # Create empty data frame for later use
  euclideanNormAcceleration <- data.frame(matrix(nrow = 0, ncol = 1))

  # Compute Euclidean norm of sums of accelleration vectors
  for(i in 1:(nrow(acc_x)-1)){
    # Formula: sqrt(accelerationx(t+1) - accelerationx(t))^2 +
    # (accelerationy(t+1) - accelerationy(t))^2)
    euclideanNormAcceleration <- rbind(euclideanNormAcceleration,
                                   sqrt(sum(as.matrix(acc_x[i,])^2,
                                            as.matrix(acc_y[i,])^2)))
  }

  # Return result
  euclideanNormAcceleration <- setNames(euclideanNormAcceleration,
                                        "Euclidean_norm_acceleration")
  return(euclideanNormAcceleration)
}
