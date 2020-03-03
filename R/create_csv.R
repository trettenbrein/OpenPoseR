# create_csv()
#
# OpenPoseR (https://github.com/trettenbrein/OpenPoseR)
# Patrick C. Trettenbrein, trettenbrein@cbs.mpg.de
#
# Creates a CSV of 2D keypoints containing all values (x,y,c) for one frame per row.
# Creates separate output files for the different models (BODY25, hands, face). De-
# faults to dedecting the data available from different models or uses "model" argu-
# ment.

create_csv <- function(input_path, filename, output_path = input_path,
                       model = "all"){
  # We at least need an input path to know where the data is located
  if(missing(input_path)) {
    stop("Argument \"input_path\" must be specified. Path to JSON (*.json) files, including trailing \"/\".",
         call. = FALSE)
  }

  # We'll need the RJSON package for this to work
  if (!requireNamespace("rjson", quietly = TRUE)) {
    stop("Package \"rjson\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else {
    # Load "rjson" package
    library("rjson")
  }

  # Check if output  directory exists; if not, create it
  if(!file.exists(output_path)) {
    mkdir(output_path)
  }

  # Path for input and output files
  #filesdir <- paste(input_path, filename, sep = "")

  # List JSON files in video folder (1 files per frame)
  files <- list.files(path = input_path, full.names = TRUE, pattern = ".json")

  if(length(files)==0){
    stop(paste("Couldn't find any JSON files in directory: ", input_path, sep = ""),
         call. = FALSE)
  }

  # Create empty data frames (just in case for hands and face models)
  output_body25 <- data.frame()
  output_hand_left <- data.frame()
  output_hand_right <- data.frame()
  output_face <- data.frame()

  # Loop through list of files and add one row per file/frame to data frame
  for(f in files){
    # Read data from JSON file for this frame
    json_data <- fromJSON(file=f)

    if(model=="all" | model=="body25") {
      # Add row to data frame for this clip for body pose model
      output_body25 <- rbind(output_body25,
                             t(json_data[["people"]][[1]][["pose_keypoints_2d"]]))
    }

    # Check whether hand model was also used and output should be included
    if(length(json_data[["people"]][[1]][["hand_left_keypoints_2d"]]) &
       length(json_data[["people"]][[1]][["hand_right_keypoints_2d"]]) > 0 &
       model=="all" | model=="hands"){
      output_hand_left <- rbind(output_hand_left,
                                t(json_data[["people"]][[1]][["hand_left_keypoints_2d"]]))
      output_hand_right <- rbind(output_hand_right,
                                 t(json_data[["people"]][[1]][["hand_right_keypoints_2d"]]))
    }

    # Check whether face model was also used and if output should be included
    if(length(json_data[["people"]][[1]][["face_keypoints_2d"]]) > 0 &
       model=="all" | model=="face"){
      output_face <- rbind(output_face, t(json_data[["people"]][[1]][["face_keypoints_2d"]]))
    }
  }

  # Generate and assign column names to data frames (format is x,y,c per keypoint)
  if(model=="all" | model=="body25"){
    column_names <- unlist(strsplit(sprintf("x%1$d-y%1$d-c%1$d", seq(0, 24)), "-"))
    output_body25 <- setNames(output_body25, column_names)
  }
  if(model=="all" | model=="hands"){
    column_names_hands <- unlist(strsplit(sprintf("x%1$d-y%1$d-c%1$d", seq(0, 20)),
                                          "-"))
    if(length(output_hand_left) & length(output_hand_right) > 0){
      output_hand_left <- setNames(output_hand_left, column_names_hands)
      output_hand_right <- setNames(output_hand_right, column_names_hands)
    }
  }
  if(model=="all" | model=="face"){
    column_names_face <- unlist(strsplit(sprintf("x%1$d-y%1$d-c%1$d", seq(0, 69)),
                                         "-"))
    if(length(json_data[["people"]][[1]][["face_keypoints_2d"]]) > 0){
      output_face <- setNames(output_face, column_names_face)
    }
  }

  # Write output to CSV ("filename_MODEL.csv" in "outputdir")
  write.csv(output_body25, file = paste(output_path, "/", filename,
                                        "_body25.csv", sep=""), row.names = FALSE)
  if(length(output_hand_left) & length(output_hand_right) >0){
    write.csv(output_hand_left, file = paste(output_path, "/", filename,
                                             "_hand_left.csv", sep=""),
                                             row.names = FALSE)
    write.csv(output_hand_right, file = paste(output_path, "/", filename,
                                              "_hand_right.csv", sep=""),
                                              row.names = FALSE)
  }
  if(length(json_data[["people"]][[1]][["face_keypoints_2d"]]) > 0){
    write.csv(output_face, file = paste(output_path, "/", filename,
                                        "_face.csv", sep=""), row.names = FALSE)
  }
}
