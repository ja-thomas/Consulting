library(mlr)
library(data.table)
library(gbm)

load("../Data/data_full_ts_z.RData")



data_full <- ddply(data_full, ~joint_Nr + course_Id + person + sensorId,
                   function(s){
                     s <- data.table(s)
                     s <- s[order(timestamp)]
                     s[,acceleration_diff := c(NA, diff(acceleration))]
                   })

data_full <- data.table(data_full)
setkey(data_full, joint_Nr, course_Id, person, sensorId, timestamp)

data_full[,abs_dist := abs(2.5 - camera_distance)]

data_sub <- data_full[.(0)]

data_sub <- as.data.frame(data_sub)
data_sub <-  data_sub[-which(is.nan(data_sub$azimut)),]



data <- makeRegrTask(id = "data_sub", data=data_sub[,c("deviation","abs_dist", "acceleration", "acceleration_diff",
                                       "shoulder_angle", "azimut", "elevation", "bone_error", "kinect_error",
                                       "forecast_x", "forecast_y", "forecast_z", "z_fraction")] , target = "deviation")



ps = makeParamSet(
  makeNumericParam("n.trees", lower = 500, upper = 10000, default = 2000),
  makeDiscreteParam("interaction.depth", values = c(1,2,3)),
  makeDiscreteParam("distribution", values = c("gaussian", "laplace", "tdist")),
  makeNumericParam("shrinkage", lower = 0.005, upper = 0.05, default = 0.01)
)

ctrl = makeTuneControlIrace(maxExperiments = 200L)

rdesc = makeResampleDesc("Holdout")

res = tuneParams("regr.gbm", data, rdesc, par.set = ps, control = ctrl, show.info = TRUE)

save("../Data/gbm_tuned_joint_0.RData")


