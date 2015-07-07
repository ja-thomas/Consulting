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

data_full[,abs_dist := abs(2.5 - distance)]





data <- makeRegrTask(data=data_full[.(0)][,c("deviation","abs_dist", "acceleration", "acceleration_diff",
                                       "shoulder-angle", "azimut", "elevation", "bone_error",
                                       "forecast_x", "forecast_y", "forecast_z", "z_fraction"),with = FALSE] , target = "deviation")



ps = makeParamSet(
  makeNumericParam("n.trees", lower = 500, upper = 10000, default = 2000),
  makeDiscreteParam("interaction.deph", values = c(1,2,3)),
  makeDiscreteParam("distribution", values = c("gaussian", "laplace", "tdist")),
)

ctrl = makeTuneControlIrace(maxExperiments = 200L)

rdesc = makeResampleDesc("Holdout")

res = tuneParams("regr.gbm", data, rdesc, par.set = ps, control = ctrl, show.info = TRUE)

save("../Data/gbm_tuned_joint_0.RData")


