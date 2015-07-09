library(data.table)
library(plyr)

load("../Data/data_full_ts_z.RData")
load("../Data/Bone_error_akt.RData")

data_full <- data.table(data_full)
setkey(data_full, joint_Nr, course_Id, person, sensorId, timestamp)
setnames(data_full, "bone_error", "bone_error_bad")


Knochenvariable$timestamp <- as.character(Knochenvariable$timestamp)
Knochenvariable$sensorId <- as.character(Knochenvariable$sensorId)
Knochenvariable$person <- as.character(Knochenvariable$person)
Knochenvariable$course_Id <- as.character(Knochenvariable$course_Id)
Knochenvariable <- data.table(Knochenvariable)
setkey(Knochenvariable, joint_Nr, course_Id, person, sensorId, timestamp)



data_full_2 <- merge(data_full, Knochenvariable, 
                   by = c("joint_Nr","course_Id", "person", "sensorId", 
                          "timestamp"))
rm(data_full)
setnames(data_full_2, "azimut", "periphery_x")
setnames(data_full_2, "elevation", "periphery_y")
data_full_2[, bone_error_bad := NULL]

data_full <- ddply(data_full_2, ~joint_Nr + course_Id + person + sensorId,
                   function(s){
                     s <- data.table(s)
                     s <- s[order(timestamp)]
                     s[,acceleration_diff := c(NA, diff(acceleration))]
                   })

data_full <- data.table(data_full)
setkey(data_full, joint_Nr, course_Id, person, sensorId, timestamp)

data_full[,ID := 1:nrow(data_full)]
data_full[,abs_dist := abs(2.5 - camera_distance)]

rm(data_full_2)
rm(Knochenvariable)

save(data_full, file = "../Data/data_complete.RData")
