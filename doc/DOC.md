# OpenPoseR Documentation

This page provides a very basic guide to the functionality provided by OpenPoseR, illustrating how the different functions provided were intended to be used.

## What is OpenPoseR?

An [R](https://www.r-project.org) package that provides some functions for analyzing motion-tracking data derived from video files using [OpenPose](https://github.com/CMU-Perceptual-Computing-Lab/openpose).

The original motivation for creating this package was to control video stimuli in sign language and gesture reserach, but the provided functionality may also be useful for other purposes.

## Installation  

For now, OpenPoseR can be installed using the following commands (you will need to have the ``devtools`` package installed):

```r
# Install devtools from CRAN (if not already installed)
install.packages("devtools")

# Install OpenPoseR package from Github
devtools::install_github("trettenbrein/OpenPoseR")
```

## How to use

To be added.

### Data structure

x0       |       y0 |       c0 |       x1 |       y1 | &hellip;
---------|----------|----------|----------|----------|---------
362.312  |  119.557 | 0.913335 |  362.388 |  233.906 | &hellip;
362.307  |  119.561 | 0.912487 |  362.390 |  233.929 | &hellip;
&hellip; | &hellip; | &hellip; | &hellip; | &hellip; | &hellip;
