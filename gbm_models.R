library(gbm)
library(data.table)
library(plyr)
library(foreach)
library(doMC)


load("../Data/data_full_ts.RData")

doMC::registerDoMC(cores = 5)
gbm_models <- llply(0:24, function(j) 
  gbm(formula = deviation ~ kinect_error + abs(camera_distance -2.5) +
       position_change + acceleration + c(NA, diff(acceleration)) + 
       shoulder_angle + azimut + elevation + bone_error + forecast_x + 
       forecast_y + forecast_z, data = data_full[.(j)],
     distribution = "gaussian",  n.trees = 5000, interaction.depth = 1), 
  .parallel = TRUE)


save(gbm_models, file = "../Data/gbm_models.RData")



