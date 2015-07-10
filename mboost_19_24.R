library(mboost)
library(party)
library(data.table)
library(plyr)
library(foreach)
library(doMC)

load("../Data/data_complete.RData")


ctrl <- ctree_control(maxdepth = 2)

for(j in 19:24){
  cat(paste("Start mboost model for joint", j, "at:", Sys.time(), "\n"))
  
  mboost_model <- mboost(deviation ~ 
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
                         data = as.data.frame(data_full[.(j)]),
                         family = GammaReg(),
                         control = boost_control(mstop = 5000,
                                                 nu = 0.1,
                                                 trace = TRUE))
  
  print(mboost_model)
  
  save(mboost_model, file = paste0("../Data/mboost_big_model_joint_",j,".RData"))
  cat(paste("finished mboost model for joint", j, "at:", Sys.time(), "\n\n"))
  
}



