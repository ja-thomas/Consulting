library(data.table)
library(plyr)
library(foreach)
library(doMC)


load("../Data/data_complete.RData")


#remove some outliers for numeric stability

data_full <- data_full[deviation < quantile(deviation, 0.9999) & 
            acceleration < quantile(acceleration, 0.999, na.rm = TRUE) &
            acceleration_diff < quantile(acceleration_diff, 0.999, na.rm = TRUE) &
            bone_error < quantile(bone_error, 0.9999) &
            abs_dist < quantile(abs_dist, 0.9999), ]



cross_validate_join_gamma <- function(one_joint_frame, ...){
  
  ddply(one_joint_frame, ~course_Id + person, 
        function(test_set){
          
          train_index <- setdiff(one_joint_frame$ID, test_set$ID)      
          
          train_set <- one_joint_frame[one_joint_frame$ID %in% train_index, ]
          
          model <- glm(formula = deviation ~ kinect_error + 
                         abs_dist + 
                         acceleration + acceleration_diff + 
                         shoulder_angle + periphery_x + periphery_y + 
                         bone_error + forecast_x + forecast_y + 
                         forecast_z + z_fraction, 
                       data = train_set, ...)
          
          test_set$pred_deviation_log_gamma <- predict.glm(model,
                                                           test_set,
                                                           type = "response")
          
          test_set
          
        })
}


registerDoMC(cores = 5)
data_full_predicted <- ddply(data_full, ~joint_Nr, 
                             cross_validate_join_gamma, family = Gamma(link = "log"),
                             .parallel = TRUE)

save(data_full_predicted, file = "../Data/data_full_gamma_glm.RData")

