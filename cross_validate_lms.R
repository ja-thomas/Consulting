library(data.table)
library(plyr)
library(foreach)

library(doMC)


load("../Data/data_full_ts_z.RData")



data_full <- ddply(data_full, ~joint_Nr + course_Id + person + sensorId,
      function(s){
        s <- data.table(s)
        s <- s[order(timestamp)]
        s[,acceleration_diff := c(NA, diff(acceleration))]
      })

data_full <- data.table(data_full)
setkey(data_full, joint_Nr, course_Id, person, sensorId, timestamp)

data_full[,ID := 1:nrow(data_full)]
data_full[,abs_dist := abs(2.5 - camera_distance)]

cross_validate_join <- function(one_joint_frame){
  
  ddply(one_joint_frame, ~course_Id + person + sensorId, 
        function(test_set){
    
    train_index <- setdiff(one_joint_frame$ID, test_set$ID)      
          
    train_set <- one_joint_frame[one_joint_frame$ID %in% train_index, ]
    
    model <- lm(formula = deviation ~ kinect_error + 
                  abs(camera_distance -2.5) + 
                  acceleration + acceleration_diff +
                  z_fraction + bone_error +
                  shoulder_angle + azimut + elevation + 
                  forecast_x + forecast_y + forecast_z, 
                data = train_set)
    
    test_set$predicted_deviation <- predict.lm(model,
                                                test_set)
    
    test_set
    
  })
}

registerDoMC(cores = 4)
data_full_predicted <- ddply(data_full, ~joint_Nr, cross_validate_join,
                             .parallel = TRUE)


save(data_full_predicted, file = "../Data/data_full_lm.RData")

