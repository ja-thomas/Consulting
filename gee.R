library(gee)
library(data.table)
library(plyr)

load("../Data/data_full_ts.RData")

data_full[order(joint_Nr)]


gee_model <- tryCatch(gee(formula = deviation ~ kinect_error + 
                   abs(camera_distance -2.5) + position_change + 
                   acceleration + c(NA, diff(acceleration)) + 
                   shoulder_angle + azimut + elevation + bone_error + 
                   forecast_x + forecast_y + forecast_z, 
                 id = joint_Nr, data = data_full, family = gaussian, 
                 corstr = "unstructured"),
                 error = function(e) NA)

save(gee_model, file = "../Data/gee_model_unstructured.RData")




subs <- data_full[,c("deviation", "joint_Nr"), with = FALSE]


x <- lapply(0:24, function(j) subs[.(j)]$deviation)

z <- do.call(cbind, x)

colnames(z) <- paste0("joint_", 0:24)
correlation <- cor(z)
save(correlation, file = ("../Data/estimated_correlation.RData"))

gee_model_fixed <- tryCatch(gee(formula = deviation ~ kinect_error + 
                   abs(camera_distance -2.5) + position_change + 
                   acceleration + c(NA, diff(acceleration)) + 
                   shoulder_angle + azimut + elevation + bone_error + 
                   forecast_x + forecast_y + forecast_z, 
                 id = joint_Nr, data = data_full, family = gaussian, 
                 corstr = "fixed", R = correlation),
                 error = function(e) NA)


save(gee_model_fixed, file = "../Data/gee_model_fixed.RData") 

