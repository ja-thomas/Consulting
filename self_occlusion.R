#library(MoCap)
library(data.table)
library(plyr)
library(foreach)
library(doMC)
#setwd("Uni/Consulting/Code/")

load("../Data/data_full_ts.RData")
source("distanceBetweenLines.R")
data <- data[,timestamp:=as.character(timestamp)]
setkey(data, joint_Nr, course_Id, person, sensorId, timestamp)

calculate_self_occlusion <- function(reference_joint, data){
  joint_segment <- list(start = c(0,0,0), end = reference_joint[,c(position_x, position_y, position_z)])
  
  other_segments <- create_bone_segment_list(reference_joint, data)
  
  distances <- lapply(other_segments, function(segment) 
    distanceBetweenLines(segment[[1]], segment[[2]], joint_segment[[1]], joint_segment[[2]])$distance)
  
  sum(1/(unlist(distances) + 0.000001))
  
}



create_bone_segment_list <- function(reference_joint, data){
  
  selected_joint <- joint_numbers[which(joint_numbers$Number == reference_joint$joint_Nr),]$Joint
  
  row_col_selected_joint <- which(colnames(kinematic_model) == selected_joint)
  
  reduced_model <- kinematic_model[-row_col_selected_joint,-row_col_selected_joint]
  
  reduced_model <- reduced_model & upper.tri(reduced_model)
  
  segment_list <- lapply(1:24, create_bone_segments, model = reduced_model, reference_joint = reference_joint, data = data)
  
  unlist(segment_list, recursive = FALSE)

}

x <- create_bone_segment_list(data[1,], data)

create_bone_segments <- function(index, model, reference_joint, data){
  
  connected_joints <- colnames(model)[model[index,]]
  end_joint <- colnames(model)[index]
  
  connected_joints <- joint_numbers[which(joint_numbers$Joint %in% connected_joints),]$Number
  end_joint <- joint_numbers[which(joint_numbers$Joint %in% end_joint),]$Number

  if(length(connected_joints) > 0){
    lapply(connected_joints, create_single_bone_segment, end_joint = end_joint, reference_joint = reference_joint, data = data)
  }
  else{
    list()
  }
}


create_single_bone_segment <- function(joint, end_joint, reference_joint, data){
  
  start <- data[.(joint, reference_joint$course, reference_joint$person, reference_joint$sensorId, reference_joint$timestamp),
                c(position_x, position_y, position_z)]
  
  end <- data[.(end_joint, reference_joint$course, reference_joint$person, reference_joint$sensorId, reference_joint$timestamp),
              c(position_x, position_y, position_z)]
  
  list(start = start, end = end)
}


registerDoMC(cores = 5)
data_full$self_occlusion <- laply(1:nrow(data_full), 
                        function(i) calculate_self_occlusion(data_full[i,], 
                                                            data = data_full), 
                        .parallel = TRUE)

save(data_full, file ="../Data_full_occlusion.RData")




