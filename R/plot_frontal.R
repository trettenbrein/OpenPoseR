# plot_frontal()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# Plot data derived from single-subject frontal tracking (e.g., stimuli)

plot_frontal <- function(plotData, width = 720, height = 576, backgroundImage = NULL, quadrantLabels = TRUE, quadrantLabelSize = 12, binSize = 125) {
  # We'll be using ggplot2 for this, so let's require it
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.", call. = FALSE)
  } else {
    # Load "ggplot2" package
    library("ggplot2")
  }

  # We'll be using ggpubr for this, so let's require it
  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop("Package \"ggpubr\" needed for this function to work. Please install it.", call. = FALSE)
  } else {
    # Load "ggpubr" package
    library("ggpubr")
  }

  # We'll be using hexbin (for computing the bins) , so let's require it
  if (!requireNamespace("hexbin", quietly = TRUE)) {
    stop("Package \"hexbin\" needed for this function to work. Please install it.", call. = FALSE)
  } else {
    # Load "hexbin" package
    library("hexbin")
  }

  # We'll be using RColorBrewer for this, so let's require it
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Package \"RColorBrewer\" needed for this function to work. Please install it.", call. = FALSE)
  } else {
    # Load "RColorBrewer" package
    library("RColorBrewer")
  }

  # We'll be using magick for this, so let's require it
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package \"magick\" needed for this function to work. Please install it.", call. = FALSE)
  } else {
    # Load "magick" package
    library("magick")
  }

  # If a background image was passed, use it; otherwise load default
  if(!is.null(backgroundImage)) {
    bg <- backgroundImage
  } else {
    # Load background image using "png" package
    bg <- magick::image_read(system.file("extdata/background.png",
                                         package = "OpenPoseR"))
  }

  # See if data is here and prepare it by renaming columns (if necessary)
  if(missing(plotData)) {
    stop("You have to pass the function some data frame containg all the points that should be plotted.",
         call. = FALSE)
  }

  if(ncol(plotData)==2) {
    colnames(plotData) <- c("x", "y")
  } else {
    stop("Please pass a dataframe to the function that contains only two columns (x, y) containing all the points that should be plotted.", call. = FALSE)
  }

  # Colors:
  rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
  r <- rf(32)

  # Create plot
  plot <- ggplot(plotData, aes(x,y)) +
    geom_vline(colour="#6c6c6c", xintercept=360, linetype = "dotted") +
    geom_hline(colour="#6c6c6c", yintercept=288, linetype = "dotted") +
    background_image(bg)+
    xlim(0,width) +
    ylim(height,0) +
    labs(fill = "Count") +
    theme_void() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "none")

  if(quadrantLabels==TRUE) {
    plot <- plot +
      annotate(geom="text", x=0, y=20, label="1",
               color="#6c6c6c", size = quadrantLabelSize) +
      annotate(geom="text", x=700, y=20, label="2",
               color="#6c6c6c", size = quadrantLabelSize) +
      annotate(geom="text", x=0, y=556, label="3",
               color="#6c6c6c", size = quadrantLabelSize) +
      annotate(geom="text", x=700, y=556, label="4",
               color="#6c6c6c", size = quadrantLabelSize) +
      annotate(geom="text", x=180, y=576, label="dexter",
               color="#6c6c6c", size = 4) +
      annotate(geom="text", x=540, y=576, label="sinister",
               color="#6c6c6c", size = 4) +
      annotate(geom="text", x=0, y=144, label="superior",
               color="#6c6c6c", size = 4, angle = 90) +
      annotate(geom="text", x=0, y=432, label="inferior",
               color="#6c6c6c", size = 4, angle = 90)
  }

  plot <- plot + stat_bin_hex(bins=binSize) + scale_fill_gradientn(colours=r)

  return(plot)
}
