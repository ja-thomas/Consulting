#This takes several Hours to run
library(devtools)
load_all("MoCap/")
library(data.table)
library(parallel)


paths_reg <- read.table("../registered_paths.txt", sep = "\t", 
                    stringsAsFactors = FALSE)
paths_unreg <- read.table("../unregistered_paths.txt", sep = "\t", 
                         stringsAsFactors = FALSE)

setwd("../Data/")
full_data <- as.list(rep(NA, nrow(paths_reg)))



get_all_joints <- function(id, data, joints = 0:24, person, course){
  kinect <- select_sensor(data, id)
  result <- do.call(rbind, lapply(joints, function(j)
    cbind(select_joint(cast = kinect, joint = j, include_error = TRUE),
          joint_Nr = j)))
  data.table(result, sensorId = id, person = person, course = course,
             key = c("joint_Nr", "timestamp", "person", "course", "sensorId"))
}


for(i in 1:nrow(paths_reg)){
  data_reg <- read_motion_data(paste0(".", paths_reg[i,1]))
  data_unreg <- read_motion_data(paste0(".", paths_unreg[i,1]))
  
  person <- unlist(strsplit(paths_reg[i,1], "/"))[4]
  course <- unlist(strsplit(paths_reg[i,1], "/"))[5]
  
  ids <- setdiff(data_reg$sensorIds, "OPTITRACK")
  
  
  
  
  data_reg <- mclapply(ids, function(id) do.call(rbind, lapply(0:24, 
                                                             create_deviation_data,
                                                             select_sensor(data_reg, id),
                                                             select_sensor(data_reg, 
                                                                           "OPTITRACK"),
                                                             person = person,
                                                             course = course)),
                     mc.cores = 2)
  
  data_reg <- do.call(rbind, data_reg)
    
  data_unreg <- mclapply(ids, get_all_joints, data = data_unreg, person = person, 
                     course = course, mc.cores = 2)
  
  data_unreg <- do.call(rbind, data_unreg)
  
  data_n <- merge.data.frame(data_reg, data_unreg, 
                  by = c("timestamp", "joint_Nr", "sensorId", "person", "course"))
  
  
  full_data[[i]] <- data_n
  write.csv(data_n, paste0("data_", person, "_", course, ".csv"),
            row.names = FALSE) 
  print(i)
}

full_data <- do.call(rbind, full_data)


full_data <- data.table(full_data)
setnames(full_data, "Value.Error", "kinect_error")
setnames(full_data, "Value.position.X", "position_x")
setnames(full_data, "Value.position.Y", "position_y")
setnames(full_data, "Value.position.Z", "position_z")

full_data[,camera_distance := sqrt(position_x^2 + position_y^2 + position_z^2)]
setkey(full_data, joint_Nr, course, person, sensorId, timestamp)

save(full_data, file="full_data.RData")
write.csv(full_data, "data_full.csv",row.names = FALSE)


data <- full_data[!is.na(deviation)]
save(data, file="data.RData")
write.csv(data, "data.csv", row.names = FALSE)





