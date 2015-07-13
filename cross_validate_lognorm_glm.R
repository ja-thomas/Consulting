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



cross_validate_join_gamma <- function(one_joint_frame,...){
  
  ddply(one_joint_frame, ~course_Id + person, 
        function(test_set){
          cat(paste(unique(test_set$person), unique(test_set$course_Id), "\n\n"))
          train_index <- setdiff(one_joint_frame$ID, test_set$ID)      
          
          train_set <- one_joint_frame[one_joint_frame$ID %in% train_index, ]
          
          tryCatch({model <- glm(formula = deviation ~ kinect_error + 
                                   abs_dist + 
                                   acceleration + acceleration_diff + 
                                   shoulder_angle + periphery_x + periphery_y + 
                                   bone_error + forecast_x + forecast_y + 
                                   forecast_z + z_fraction, 
                                 data = train_set, ...)
          
          test_set$pred_deviation_log_norm <- predict.glm(model,
                                                           test_set,
                                                           type = "response")
          cat("\n\n")
          test_set},
          error = function(e) cat("\n skipped \n"))
          
          
          
        })
}




data_full_predicted <- list()

for(i in 0:24){
  cat(paste("joint:", i, "\n"))
  
  data_full_predicted[(i+1)] <- cross_validate_join_gamma(data_full[.(i)],
                                                          family = gaussian(link = "log"),
                                                          control = list(trace = TRUE,
                                                                         maxit = 100))
  cat("\n\n")
}


save(data_full_predicted, file = "../Data/data_full_gamma_glm.RData")


