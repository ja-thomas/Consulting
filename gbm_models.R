library(gbm)
library(data.table)
library(plyr)
library(foreach)
library(doMC)


load("../Data/data_full_ts.RData")

doMC::registerDoMC(cores = 4)
gbm_models <- llply(0:24, function(j) 
  tryCatch(gbm(formula = deviation ~ kinect_error + abs(camera_distance -2.5) +
       position_change + acceleration + c(NA, diff(acceleration)) + 
       shoulder_angle + azimut + elevation + bone_error + forecast_x + 
       forecast_y + forecast_z, data = data_full[.(j)],
     distribution = "gaussian",  n.trees = 10000, interaction.depth = 1,
     cv.folds = 5), error = function(e) NA), .parallel = TRUE)


save(gbm_models, file = "../Data/gbm_models_bigger.RData")



