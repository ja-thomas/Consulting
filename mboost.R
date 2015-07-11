library(mboost)
library(party)
library(data.table)
library(plyr)
library(foreach)
library(doMC)

load("../Data/data_complete.RData")

calculate_mboost_model <- function(data_one_joint){
  
  ctrl <- ctree_control(maxdepth = 2)
  
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
                         data = data_one_joint,
                         family = GammaReg(),
                         control = boost_control(mstop = 10000,
                                                 nu = 0.1))
  
  cv <- cvrisk(mboost_model, folds = cv(model.weights(mboost_model), 
                                                      type = "kfold",
                                                      B = 3),
               papply = lapply)
  
  
  list(cbind(
    data = data_one_joint[,c("deviation","timestamp", "sensorId", 
                             "person", "course_Id", "joint_Nr"),],
    predicted_mboost = predict(mboost_model, type = "response")),
  variable_importance = mboost_model$xselect(),
  aic = AIC(mboost_model),
  cv = cv)
}


registerDoMC(cores = 10)
mboost_models <- dlply(data_full, ~joint_Nr, calculate_mboost_model,
                  .parallel = TRUE)

save(mboost_models, file = "../Data/mboost_model_summary.RData")



  
