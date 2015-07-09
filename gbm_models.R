library(gbm)
library(data.table)
library(plyr)

setwd("Uni/Consulting/Code/")
load("../Data/data_complete.RData")

data_full <- data_full[,c("timestamp", "sensorId","person","course_Id", 
                         "joint_Nr","deviation",  "kinect_error", 
                         "acceleration", "shoulder_angle", "periphery_x", 
                         "periphery_y", "forecast_x", "forecast_y", 
                         "forecast_z", "z_fraction", "bone_error", 
                         "acceleration_diff","abs_dist"), with=FALSE]

data_full <- as.data.table(data_full)
setkey(data_full, joint_Nr)


for(j in 3:24){
  cat(paste("Start gbm model for joint", j, "at:", Sys.time(), "\n"))
  gbm_model <- gbm(formula = log(deviation) ~ kinect_error + 
                              abs_dist + 
                              acceleration + acceleration_diff + 
                              shoulder_angle + periphery_x + periphery_y + 
                              bone_error + forecast_x + forecast_y + 
                              forecast_z + z_fraction, 
                            data = as.data.frame(data_full[.(j)]),
                            distribution = "gaussian",  n.trees = 5000, 
                            interaction.depth = 2, cv.folds = 2,
                            shrinkage = 0.1)
  print(gbm_model)

  save(gbm_model, file = paste0("../Data/gbm_big_model_joint_",j,".RData"))
  rm(gbm_model)
  gc()
  cat(paste("finished gbm model for joint", j, "at:", Sys.time(), "\n\n"))
}
  





