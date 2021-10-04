# en_velocity()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
# Emiliano Zaccarella, zaccarella@cbs.mpg.de
#
# Compute Euclidean norm of sums of velocity vectors for a tracked video using the
# x-axis and y-axis data generated using velocity_x() and velocity_y().

en_velocity <- function(vel_x, vel_y, start_from_zero = TRUE) {
  # We at least need some data to work with
  if(missing(vel_x) | missing(vel_y)) {
    stop("You have to pass the function two data frames (velocity on x-axis and y-axis) to work with.",
         call. = FALSE)
  }

  # Create empty data frame for later use
  euclideanNormVelocity <- data.frame(matrix(nrow = 0, ncol = 1))

  # Check whether the output should start with a row of 0s; by default it is included
  if(start_from_zero==TRUE) {
    euclideanNormVelocity[1,] <- 0
  }

  # Compute Euclidean norm of sums of velocity vectors
  for(i in 1:(nrow(vel_x)-1)){
    # Formula: sqrt(velocityx(t+1) - velocityx(t))^2 +
    # (velocityy(t+1) - velocityy(t))^2)
    euclideanNormVelocity <- rbind(euclideanNormVelocity,
                                   sqrt(sum(as.matrix(vel_x[i,])^2,
                                            as.matrix(vel_y[i,])^2)))
  }

  # Return result
  euclideanNormVelocity <- setNames(euclideanNormVelocity, "Euclidean_norm_velocity")
  return(euclideanNormVelocity)
}
