library(mboost)
library(party)
library(data.table)
library(plyr)
library(foreach)
library(doMC)

load("../Data/data_complete.RData")


ctrl <- ctree_control(maxdepth = 2)
registerDoMC(cores = 5)
mboost_models <- dlply(data_full, ~joint_Nr, function(set){
  mboost(deviation ~ 
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
         data = set,
         family = GammaReg(),
         control = boost_control(mstop = 5000,
                                 nu = 0.1))}, 
  .parallel = TRUE)
  
  
save(mboost_models, file = "../Data/mboost_models.RData")