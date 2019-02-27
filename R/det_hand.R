# det_hand()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# Determine wheter the left, right, or both arms moved. The idea is two determine
# whether a sign/gesture is one or two-handed (in clips of single signs). May also
# be used to determine the hand that was primarily used in longer clips of discourse.
# Currently only uses the data from the BODY25 model.

det_hand <- function(vel_x, vel_y) {
  # We at least need some data to work with
  if(missing(vel_x) | missing(vel_y)) {
    stop("You have to pass the function two data frames (velocity on x-axis and y-axis) to work with.",
         call. = FALSE)
  }

  # Create empty data frame for later use
  left_euclideanNormVelocity <- data.frame(matrix(nrow = 0, ncol = 1))
  right_euclideanNormVelocity <- data.frame(matrix(nrow = 0, ncol = 1))

  # Which points do we actually include here?
  # 2, 3, & 4 (right arm), and 5, 6, & 7 (left arm)
  left_points_x <- paste("x", 5:7, sep = "")
  left_points_y <- paste("y", 5:7, sep = "")
  right_points_x <- paste("x", 2:4, sep = "")
  right_points_y <- paste("y", 2:4, sep = "")


  # Compute Euclidean norm of sums of velocity vectors for arm points only
  for(i in 1:(nrow(vel_x)-1)){
    # Formula: sqrt(velocityx(t+1) - velocityx(t))^2 +
    # (velocityy(t+1) - velocityy(t))^2)
    left_euclideanNormVelocity <- rbind(left_euclideanNormVelocity,
                                   sqrt(sum(as.matrix(vel_x[i,left_points_x])^2,
                                            as.matrix(vel_y[i,left_points_y])^2)))
    right_euclideanNormVelocity <- rbind(right_euclideanNormVelocity,
                                        sqrt(sum(as.matrix(vel_x[i,right_points_x])^2,
                                                 as.matrix(vel_y[i,right_points_y])^2)))
  }

  # Was one arm used more than the other?
  # Use WS test b/c data are not normally distributed
  comparison <- wilcox.test(left_euclideanNormVelocity[,1],
                    right_euclideanNormVelocity[,1], paired = TRUE)

  # Were both arms moved in a very similar way? i.e. is the tracking data correlated
  # This can be interpreted to indicate that the tracked motion was symmetrical
  correlation <- cor.test(left_euclideanNormVelocity[,1],
                          right_euclideanNormVelocity[,1])

  # Is there a difference between both arms? If no, is there similarity?
  if(comparison$p.value<=.05) {
    # Now that we know that one arm moved sign. more, which one?
    if(sum(left_euclideanNormVelocity[,1]) > sum(right_euclideanNormVelocity[,1])) {
      arm <- "left"
    } else if(sum(left_euclideanNormVelocity[,1]) <
              sum(right_euclideanNormVelocity[,1])) {
      arm <- "right"
    }
  } else if(correlation$p.value<=.05) {
    arm <- "two"
  } else {
    arm <- NULL
  }

  # Return result
  return(arm)
}
