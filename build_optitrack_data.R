library(MoCap)
library(data.table)
options(scipen=999)

get_all_joints <- function(id, data, joints = 0:24, person, course){
  kinect <- select_sensor(data, id)
  result <- do.call(rbind, lapply(joints, function(j)
    cbind(select_joint(cast = kinect, joint = j, include_error = TRUE),
          joint_Nr = j)))
  data.table(result, sensorId = id, person = person, course = course,
             key = c("joint_Nr", "timestamp", "person", "course", "sensorId"))
}


full_data <- as.list(rep(NA, nrow(paths_reg)))

  
paths_reg <- read.table("../registered_paths.txt", sep = "\t", 
                        stringsAsFactors = FALSE)

for(i in 1:nrow(paths_reg)){
  data_reg <- read_motion_data(paste0(".", paths_reg[i,1]))
  
  person <- unlist(strsplit(paths_reg[i,1], "/"))[4]
  course <- unlist(strsplit(paths_reg[i,1], "/"))[5]
  
  
  full_data[[i]] <- get_all_joints("OPTITRACK", data = data_reg, person = person, 
                              course = course)

  print(i)
}


registered_optitrack_data <- do.call(rbind, full_data)
setnames(registered_optitrack_data, "course", "course_Id")
setnames(registered_optitrack_data, "Value.position.X", "position_x")
setnames(registered_optitrack_data, "Value.position.Y", "position_y")
setnames(registered_optitrack_data, "Value.position.Z", "position_z")
save(registered_optitrack_data, 
     file = "../Data/registered_optitrack_data.RData")
