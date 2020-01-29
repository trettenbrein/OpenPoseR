# plot_timeseries()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# Plot time series data derived from single-subject tracking (e.g., stimuli)

plot_timeseries <- function(plotData, dataType = "1") {
  # We'll be using ggplot2 for this, so let's require it
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.", call. = FALSE)
  } else {
    # Load "ggplot2" package
    library("ggplot2")
  }

  # We'll be using reshape for data manipulation, so let's require it
  if (!requireNamespace("reshape", quietly = TRUE)) {
    stop("Package \"reshape\" needed for this function to work. Please install it.", call. = FALSE)
  } else {
    # Load "reshape" package
    library("reshape")
  }

  # We at least need some data to work with
  if(missing(plotData)) {
    stop("You have to pass the function at least one data frame to plot.",
         call. = FALSE)
  }

  # Check if we were told what kind of data this is (by passing a number)
  # Alternatively, we may have been passed a different label; then do nothing
  if(dataType==1) {
    dataType <- "Euclidian norm of the sums of velocity vectors"
  } else if(dataType==2) {
    dataType <- "Euclidian norm of the sums of acceleration vectors"
  }

  # We'll need a reference -> Add frame numbers (if not already there)
  if(!"d" %in% colnames(plotData)) {
    plotData$FrameNr <- seq.int(nrow(plotData))
  }

  # Create the plot
  plot <- ggplot(melt(plotData, id.vars = "FrameNr"), aes(x=FrameNr, y = value,
                                                      color = variable)) +
    geom_line() +
    xlab("Frame number") +
    ylab(dataType)

  # If the data only had one column let's remove the legend
  # But we check for two columns because we added "FrameNr" column above
  if (ncol(plotData)==2){
    plot <- plot + theme(legend.position = "none")
  }

  # Return plot
  return(plot)
}
