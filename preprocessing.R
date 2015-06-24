options(scipen = 999)

library(MoCap)
library(data.table)
library(plyr)
setwd("~/Uni/Consulting/Code/")

load("../Data/data_processed.RData")



#remove casts with negative z-Values
data <- data[!(person=="Proband 3" & course_Id %in% c("Vor zurueck","Laufen (2)","Drehen gegen UZS"))]


data[, timestamp:=as.character(timestamp)]
setkey(data, joint_Nr,course_Id, person, sensorId, timestamp)


#merge angle Values

load("../Data/angledata_new.Rdata")

angle_data$timestamp <- as.character(angle_data$timestamp)
angle_data$sensorId <- as.character(angle_data$sensorId)
angle_data$person <- as.character(angle_data$person)
angle_data$course_Id <- as.character(angle_data$course_Id)
angle_data <- data.table(angle_data)
setkey(angle_data, course_Id, person, sensorId, timestamp)


data_full <- merge(data, angle_data, by.x = c("joint_Nr","course_Id", "person", "sensorId", "timestamp"))
setkey(data_full, joint_Nr, course_Id, person, sensorId, timestamp)

#periphery

data_full[, azimut := acos(position_z/sqrt(position_x^2 + position_z^2))]
data_full[, elevation := acos(position_y/sqrt(position_y^2 + position_z^2))]


#bone length
load("../Data/Bone_error.RData")

Knochenvariable$timestamp <- as.character(Knochenvariable$timestamp)
Knochenvariable$sensorId <- as.character(Knochenvariable$sensorId)
Knochenvariable$person <- as.character(Knochenvariable$person)
Knochenvariable$course_Id <- as.character(Knochenvariable$course_Id)
Knochenvariable <- data.table(Knochenvariable)
setkey(Knochenvariable,joint_Nr, course_Id, person, sensorId, timestamp)


data_full <- merge(data_full, Knochenvariable, 
           by = c("joint_Nr","course_Id", "person", "sensorId", "timestamp"))

save(data_full, file = "../Data/data_full.RData")
