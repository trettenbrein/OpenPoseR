# smooth_timeseries()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# Applies a Kolmogorov-Zurbenko filter to the given time series data.
#
# Note: This function is inspired by and partly based upon code for
# smoothing procedures discussed by Wim Pouw (wim.pouw@donders.ru.nl)
# and James Trujillo (james.trujillo@donders.ru.nl) in the context of
# the "Envision Bootcamp" 2021. Plese find further details here:
# https://wimpouw.github.io/EnvisionBootcamp2021/MergingAcousticsMT.html

smooth_timeseries <- function(data, span = 4, order = 6) {
  # We'll be using the "kza" package, so let's require it
  if (!requireNamespace("kza", quietly = TRUE)) {
    stop("Package \"kza\" needed for this function to work. Please install it.", call. = FALSE)
  } else {
    # Load "kza" package
    library("kza")
  }

  # We at least need some data to work with
  if(missing(data)) {
    stop("You have to pass the function a data frame to work with.",
         call. = FALSE)
  }

  filtered_data <- kza(x = data, m = span, k = order, impute_tails = TRUE)
  output <- filtered_data$kza

  # Return result
  return(output)
}
