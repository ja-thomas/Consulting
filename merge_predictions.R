library(plyr)
library(data.table)

load("../Data/mboost_model_summary.RData")


data_mboost <- do.call(rbind, lapply(mboost_models, function(x) x[[1]]))
rm(mboost_models)
data_mboost <- data.table(data_mboost)
setkey(data_mboost, joint_Nr, person, course_Id, sensorId, timestamp)



load("../Data/data_complete.RData")
data_full <- merge(data_full, data_mboost)
rm(data_mboost)


load("../Data/data_full_gamma_glm.RData")


data_full_gamma <- do.call(rbind, data_full_predicted)
rm(data_full_predicted)
data_full_gamma <- data.table(data_full_gamma)
setkey(data_full_gamma, joint_Nr, person, course_Id, sensorId, timestamp)

data_full <- merge(data_full, data_full_gamma[,c("timestamp", "sensorId", 
                                                 "person", "course_Id", 
                                                 "joint_Nr", 
                                                 "pred_deviation_log_gamma")
                                              , with = FALSE],
                   all.x = TRUE, by = c("timestamp", "sensorId", 
                                        "person", "course_Id", 
                                        "joint_Nr"))
rm(data_full_gamma)


load("../Data/data_full_lognorm_glm.RData")


data_full_lognorm <- do.call(rbind, data_full_predicted)
rm(data_full_predicted)
data_full_lognorm <- data.table(data_full_lognorm)
setkey(data_full_lognorm, joint_Nr, person, course_Id, sensorId, timestamp)

data_full <- merge(data_full, data_full_lognorm[,c("timestamp", "sensorId", 
                                                 "person", "course_Id", 
                                                 "joint_Nr", 
                                                 "pred_deviation_log_norm")
                                              , with = FALSE],
                   all.x = TRUE, by = c("timestamp", "sensorId", 
                                        "person", "course_Id", 
                                        "joint_Nr"))
rm(data_full_lognorm)

save(data_full, file = "../Data/data_predictions.RData")
