library(gbm)
library(data.table)
library(plyr)

load("../Data/data_full_ts_z.RData")

data_full <- ddply(data_full, ~joint_Nr + course_Id + person + sensorId,
                   function(s){
                     s <- data.table(s)
                     s <- s[order(timestamp)]
                     s[,acceleration_diff := c(NA, diff(acceleration))]
                   })

data_full <- data.table(data_full)
setkey(data_full, joint_Nr, course_Id, person, sensorId, timestamp)

for(j in 0:24){
  print(paste("Start gbm model for joint ", j, "at:", Sys.time()))
  gbm_model <- gbm(formula = deviation ~ kinect_error + 
                              abs(camera_distance -2.5) + 
                              acceleration + c(NA, diff(acceleration)) + 
                              shoulder_angle + azimut + elevation + 
                              bone_error + forecast_x + forecast_y + 
                              forecast_z + z_fraction, 
                            data = as.data.frame(data_full[.(j)]),
                            distribution = "gaussian",  n.trees = 3000, 
                            interaction.depth = 2, cv.folds = 3,
                            shrinkage = 0.01)

  save(gbm_model, file = paste0("../Data/gbm_model_joint_",j,".RData"))
  print(paste("finished gbm model for joint ", j, "at:", Sys.time()))
}
  





