library(mboost)
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
  cat(paste("Start mboost model for joint", j, "at:", Sys.time(), "\n"))
  model <- mboost(deviation ~ 
                    btree(abs_dist, tree_controls = ctrl) + 
                    btree(acceleration, tree_controls = ctrl) + 
                    btree(kinect_error, tree_controls = ctrl) + 
                    btree(acceleration_diff, tree_controls = ctrl) + 
                    btree(shoulder_angle, tree_controls = ctrl) + 
                    btree(periphery_x, tree_controls = ctrl) + 
                    btree(periphery_y, tree_controls = ctrl) + 
                    btree(bone_error, tree_controls = ctrl) + 
                    btree(forecast_x, tree_controls = ctrl) + 
                    btree(forecast_y, tree_controls = ctrl) + 
                    btree(forecast_z, tree_controls = ctrl) +
                    btree(z_fraction, tree_controls = ctrl), 
                  data = data_full[.(j)],
                  family = GammaReg(),
                  control = boost_control(mstop = 2000,
                                          nu = 0.5,
                                          trace = TRUE))
  print(model)
  
  save(model, file = paste0("../Data/mboost_model_joint_",j,".RData"))
  rm(model)
  gc()
  cat(paste("finished gbm model for joint", j, "at:", Sys.time(), "\n\n"))
}