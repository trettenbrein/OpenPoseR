# OpenPoseR
An [R](https://www.r-project.org) package that provides functions for analyzing motion-tracking data derived from video files using [OpenPose](https://github.com/CMU-Perceptual-Computing-Lab/openpose).

The original motivation for creating this package was to control video stimuli in sign language and gesture reserach, but the provided functionality may also be useful for other purposes.

## What is this?

OpenPoseR can be used to analyze motion-tracking data derived from video files using [OpenPose](https://github.com/CMU-Perceptual-Computing-Lab/openpose). In other words, OpenPoseR does *not* provide any motion-tracking capabilities by itself. You will need to install and run OpenPose on your system first to perform the actual motion tracking analysis. Then, the OpenPoseR package provides a variety of R functions that can be used to analyse the output generated by OpenPose.

[OpenPose](https://github.com/CMU-Perceptual-Computing-Lab/openpose) is currently the most sophisticated means for tracking people in video clips. The results of motion-tracking of people in video clips with OpenPose can be used for further quantitiatve analysis that allows for quantification of movement paramters which are relevant to researchers working on sign language and gesture where bodily movements a person take on linguistic and/or discourse functions. 

[Example video](demo/data/psychologie.mp4)  |  [Example video with fit body-pose model](doc/examples_body25/psychologie_body25.mp4)
:-------------------------:|:-------------------------:
<img src="doc/psychologie.png" width="95%" /> | <img src="doc/psychologie_body25.png" width="95%" />

Example video of the German Sign Language (*Deutsche Gebärdensprache*, DGS) sign for "psychology" courtesy of [Henrike Maria Falke, gebaerdenlernen.de](http://www.gebaerdenlernen.de/index.php?article_id=88) ([Creative Commons by-nc-sa/3.0/de](https://creativecommons.org/licenses/by-nc-sa/3.0/de/)).


## What's it for?

The main reason for developing OpenPoseR was to create a state-of-the-art means for controlling for the bodily motion occuring in different video clips showing a human being either signing or using gestures captured from the front (see frame from example video of the German Sign Language sign PSYCHOLOGY with fit body-pose model above).

By quantifying the gross bodily movement of the person in a particular clip from this perspective it becomes possible to compare different clips (of the same person) that, for example, may represent different conditions in an experiment and determine their similiarty and/or differences. This method therefore may be useful for quantitative stimulus control in sign language and gesture research. <!-- See [here]() for an example.-->

<p align="center">
<img width="50%" src="doc/psychologie_timeseries.png" /><br />
OpenPoseR analysis of the data from the above clip.
</p>

It must be pointed out here that this method *cannot* and was not intended to detect the subtle differences in hand configuration, path movement, as well as non-manual components that are linguistically meaningful and essential to sign languages. The situation may be a little less dim when it comes to gesture, but similar reservations apply.

For details on what OpenPoseR can (and can't) do, respectively, how you can use OpenPoseR please see this [demo](demo/demo.pdf). [Download the demo including examples here (.zip file).](demo.zip)

## Installation  

For now, OpenPoseR (current version: 1.0) can be installed using the following commands (you will need to have the ``devtools`` package installed):

```r
# Install devtools from CRAN (if not already installed)
install.packages("devtools")

# Install OpenPoseR package from Github
devtools::install_github("trettenbrein/OpenPoseR")
```

## Creators

The OpenPoseR package was created at the [Max Planck Institute for Human Cognitive & Brain Sciences](https://www.cbs.mpg.de) by [Patrick C. Trettenbrein](http://trettenbrein.biolinguistics.eu) in collaboration with [Emiliano Zaccarella](https://www.cbs.mpg.de/employees/zaccarella) under the supervision of [Angela D. Friederici](https://www.cbs.mpg.de/employees/friederici).

If you have found a bug, please report it [here](https://github.com/trettenbrein/OpenPoseR/issues). In case you have any questions, criticism, or suggestions that do not belong into the bug tracker, please e-mail Patrick at [trettenbrein@cbs.mpg.de](mailto:trettenbrein@cbs.mpg.de).

## Citation

If you use OpenPoseR in your own work, please reference the following paper in your manuscript(s):

> Trettenbrein, P. C., & Zaccarella, E. (2021). Controlling video stimuli in sign language and gesture research: The *OpenPoseR* package for analyzing *OpenPose* motion tracking data in *R*. *Frontiers in Psychology*, 12, 628728. [https://doi.org/10.3389/fpsyg.2021.628728](https://doi.org/10.3389/fpsyg.2021.628728)

## License

The code of this project is free for use, re-use, and modification by anyone without any liability or warranty under the conditions of the [GNU General Public License v3.0](https://github.com/trettenbrein/OpenPoseR/blob/master/LICENSE).
