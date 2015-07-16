library(data.table)
library(plyr)
library(foreach)
library(doMC)
library(mboost)
library(party)


load("../Data/data_complete.RData")

cross_validate_join_boosting <- function(one_joint_frame){
  
  ddply(one_joint_frame, ~course_Id + person, 
        function(test_set){
          
          train_index <- setdiff(one_joint_frame$ID, test_set$ID)      
          
          train_set <- one_joint_frame[one_joint_frame$ID %in% train_index, ]
          
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
                            btree(z_fraction/, tree_controls = ctrl), 
                          data = data_joint_0,
                          family = GammaReg(),
                          control = boost_control(mstop = 4000,
                                                  nu = 0.5))
          
          test_set$pred_deviation_boosting <- predict(model,
                                                      test_set,
                                                      type = "response")
          
          cat(paste("joint:", unique(test_test$joint_Nr),
                    unique(test_set$proband),
                    unique(test_set$course_Id),
                    "done"))
          
          
          write(test_set, file = paste0("../Data/mboost/", 
                                        unique(test_set$proband),
                                        unique(test_set$course_Id),
                                        ".RData"))
          
        })
}

registerDoMC(cores = 10)
data_full_predicted <- d_ply(data_full, ~joint_Nr, 
                             cross_validate_join_boosting,
                             .parallel = TRUE)
