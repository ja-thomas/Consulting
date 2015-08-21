library(data.table)
library(plyr)
library(foreach)
library(doMC)
library(mboost)
library(party)


load("../Data/data_complete.RData")


data_full <- ddply(data_full, ~joint_Nr, function(x) {
  x$cv_it <- sample(c(rep(1:10, times = 34620), 1))
  x
}
)

cross_validate_join_boosting <- function(id, data, ntree){
  
  data_set <- ddply(data, ~joint_Nr, 
        function(d){
          
          train_set <- d[d$cv_it != id, ]
          test_set <- d[d$cv_it == id, ]
          
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
          
          test_set
        })
  
  save(data_set, file = paste0("../Data/mboost/prediction_Cv",id , 
                                ".RData"))
  
  data_set
  
          

}
ntree <- list(1000,
              1000,
              141,
              411,
              168,
              260,
              283,
              284,
              1000,
              278,
              366,
              219,
              99,
              195,
              216,
              233,
              242,
              202,
              1000,
              530,
              145,
              1000,
              231,
              342,
              291)

registerDoMC(cores = 10)
ldply(1:10, cross_validate_join_boosting, data = data_full, ntree = ntree,
      .parallel = TRUE)

save(data_set, file = "../Data/mboost/prediction_Cv_full.RData")

