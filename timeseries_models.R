library(MoCap)
library(data.table)
library(plyr)
library(foreach)
library(doMC)
library(forecast)

setwd("Uni/Consulting/Code/")

load("../Data/data_full.RData")




forecast_positions <- function(data){
  
  data$forecast_x <- tryCatch(as.numeric(abs(ets(data$position_x)$residuals)), 
                              error = function(e) NA)
  data$forecast_y <- tryCatch(as.numeric(abs(ets(data$position_y)$residuals)), 
                              error = function(e) NA)
  data$forecast_z <- tryCatch(as.numeric(abs(ets(data$position_z)$residuals)), 
                              error = function(e) NA)
  data
}

registerDoMC(cores = 4)
data_full <- ddply(data_full, ~joint_Nr + course_Id + person + sensorId, 
           forecast_positions, .parallel = TRUE)


data_full <- data.table(data_full)
setkey(data_full, joint_Nr,course_Id, person, sensorId, timestamp)
save(data_full, file = "../Data/data_full_ts.RData")



models <- lapply(0:24,function(j) lm(deviation ~ kinect_error 
                                     + abs(camera_distance - 2.5) 
             + acceleration + c(NA, diff(acceleration)) + shoulder_angle 
             + azimut + elevation + bone_error + forecast_x + forecast_y 
             + forecast_z, data = data_full[.(j)])) 


