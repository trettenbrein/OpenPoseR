#' Motion-tracking data from BODY25 model in OpenPose for example clip ABEND.
#'
#' Motion-tracking data (points: x,y, confidence: c) derived from fitting the
#' BODY25 model in OpenPose for the example video clip (in: doc/example_videos)
#' "ABEND.mp4".
#'
#' @format A data frame with 100 rows and 75 variables:
#' \describe{
#'   \item{x0}{Position of point 0 on x-axis in frame.}
#'   \item{x0}{Position of point 0 on y-axis in frame.}
#'   \item{c0}{Confidence rating }
#' }
#' @source \url{https://osf.io/mz8j4/}
"ABEND_body25"
