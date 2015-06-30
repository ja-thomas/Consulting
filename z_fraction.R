library(MoCap)
library(data.table)
library(plyr)
library(foreach)
library(doMC)
setwd("Uni/Consulting/Code/")

calculate_z_fraction <- function(sub, bone_frame = bones){
  
  sub <- data.table(sub)
  
  bone_frame$dist <- sqrt(
    rowSums(
      (sub[joint_Nr == bone_frame$j1, 
           c("position_x", "position_y", "position_z"), with = FALSE] - 
         sub[joint_Nr == bone_frame$j2, 
             c("position_x", "position_y", "position_z"), with = FALSE])^2))
  
  bone_frame$z_fraction <- as.numeric(
    sqrt((sub[joint_Nr == bone_frame$j1, "position_z", with = FALSE] - 
            sub[joint_Nr == bone_frame$j2, "position_z", with = FALSE])^2) /
      bone_frame$dist)
  
  bone_frame$joint_Nr <- (sub[joint_Nr == bone_frame$j1, 
                              "position_z", with = FALSE] <
                            sub[joint_Nr == bone_frame$j2, 
                                "position_z", with = FALSE]) + 1
  
  bone_frame$joint_Nr <- unlist(mapply(function(x,y) 
    bone_frame[x,y], x = 1:24, y = bone_frame$joint_Nr))
  
  merge(sub, aggregate(z_fraction ~ joint_Nr, data = bone_frame, FUN = sum), by = "joint_Nr")
}

load("../Data/data_full_ts.RData")
setkey(data_full, timestamp, course_Id, person, sensorId, joint_Nr)

load("../Data/bones.RData")

doMC::registerDoMC(cores = 4)
print("starting....")
data_full <- ddply(data_full, ~timestamp + course_Id + person + sensorId, 
                   calculate_z_fraction, .parallel = TRUE)
print("saving...")
save(data_full, file = "../Data/data_full_ts_z.RData")
print("done :)")

