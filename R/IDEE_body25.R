#' Motion-tracking data from BODY25 model in OpenPose for example clip IDEE
#'
#' Motion-tracking data (points: x,y, confidence: c) derived from fitting the
#' BODY25 model in OpenPose for the example video clip (in: doc/example_videos)
#' "IDEE.mp4".
#'
#' @format A data frame with 94 rows and 75 variables:
#' \describe{
#'   \item{x0}{Position of point 0 on x-axis in frame.}
#'   \item{x0}{Position of point 0 on y-axis in frame.}
#'   \item{c0}{Confidence rating }
#' }
#' @source \url{https://osf.io/mz8j4/}
"IDEE_body25"
