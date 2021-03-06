library(data.table)
library(plyr)
library(foreach)
library(doMC)
library(mboost)
library(party)


load("../Data/data_complete.RData")

cross_validate_join_boosting <- function(one_joint_frame, ntree){
  
  ddply(one_joint_frame, ~course_Id + person, 
        function(test_set){
          
          train_index <- setdiff(one_joint_frame$ID, test_set$ID)      
          
          train_set <- one_joint_frame[one_joint_frame$ID %in% train_index, ]
          
          ctrl <- ctree_control(maxdepth = 2)
          
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
                          data = train_set,
                          family = GammaReg(),
                          control = boost_control(mstop = ntree[[(unique(test_set$joint_Nr)+1)]],
                                                  nu = 0.1))
          
          test_set$pred_deviation_boosting <- predict(model,
                                                      test_set,
                                                      type = "response")
      
          
          
          write(test_set, file = paste0("../Data/mboost/", 
                                        unique(test_set$proband),
                                        unique(test_set$course_Id),
                                        ".RData"))
          
        })
}

ntree <- list(1744,
              2910,
              141,
              411,
              168,
              260,
              283,
              284,
              1641,
              278,
              366,
              219,
              99,
              195,
              216,
              233,
              242,
              202,
              1858,
              530,
              NA,
              2556,
              NA,
              NA,
              291)

registerDoMC(cores = 10)
data_full_predicted <- d_ply(data_full, ~joint_Nr, 
                             cross_validate_join_boosting, ntree = ntree,
                             .parallel = TRUE)
