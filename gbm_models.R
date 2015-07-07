library(gbm)
library(data.table)
library(plyr)

rg

load("../Data/data_full_ts.RData")


for(j in 0:24){
  print(paste("Start gbm model for joint ", j, "at:", Sys.time()))
  gbm_model <- tryCatch(gbm(formula = deviation ~ kinect_error + 
                              abs(camera_distance -2.5) + 
                              acceleration + c(NA, diff(acceleration)) + 
                              shoulder_angle + azimut + elevation + 
                              bone_error + forecast_x + forecast_y + 
                              forecast_z, 
                            data = as.data.frame(data_full[.(j)]),
                            distribution = "gaussian",  n.trees = 8000, 
                            interaction.depth = 2, cv.folds = 3,
                            shrinkage = 0.01), 
                        error = function(e) NA)

  save(gbm_model, file = paste0("../Data/gbm_model_joint_",j,".RData"))
  print(paste("finished gbm model for joint ", j, "at:", Sys.time()))
}
  





