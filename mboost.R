library(mboost)
library(data.table)
library(plyr)
library(foreach)
library(doMC)

load("../Data/data_complete.RData")

registerDoMC(cores = 5)
mboost_models <- dlply(data_full, ~joint_Nr, function(set){
  blackboost(formula = deviation ~ kinect_error + 
               abs_dist + 
               acceleration + acceleration_diff + 
               shoulder_angle + periphery_x + periphery_y + 
               bone_error + forecast_x + forecast_y + 
               forecast_z + z_fraction, 
             data = as.data.frame(data_full[.(j)]),
             family = GammaReg(),
             control = boost_control(mstop = 5000,
                                     nu = 0.1)
}, .parallel = TRUE)
  
  
save(mboost_models, file = "../Data/mboost_models.RData")