write_error_file <- function(data, name, weights){
  require(rjson)
  
  json_one_camera <- function(d){
    elements <- dlply(d, ~joint_Nr, function(joint_row)
      list(Key = joint_row$joint_Nr, Value = as.numeric(joint_row[,weights])))
    names(elements) <- NULL
    attributes(elements) <- NULL
    elements
  }
  
  json_all_cameras <- function(d){
    elements <- dlply(d, ~sensorId, function(sens) 
      list(errors =  json_one_camera(sens),
      sensorId = unique(sens$sensorId)))
    
    names(elements) <- NULL
    attributes(elements) <- NULL
    elements
  }
  
  error_file <- dlply(data, ~timestamp, json_all_cameras) 
  
  
  names(error_file) <- NULL
  attributes(error_file) <- NULL
  
  write(rjson::toJSON(error_file), file = paste0("../Data/Error/",name, ".err"))
  
}



library(plyr)
library(data.table)
setwd("Uni/Consulting/Code/")
load("../Data/data_predictions.RData")

data_full[is.na(data_full)] = 10

d_ply(data_full, ~person+course_Id, function(x){
  
  write_error_file(data = x, name = paste0(unique(x$person), "_" , 
                                           unique(x$course_Id), "_",
                                           "lognorm"),
                   weights = "pred_deviation_log_norm")
  
  cat(paste0(unique(x$person), "_" , 
             unique(x$course_Id), "_",
             "lognorm", " done\n"))
  
})


d_ply(data_full, ~person+course_Id, function(x){
  
  write_error_file(data = x, name = paste0(unique(x$person), "_" , 
                                           unique(x$course_Id), "_",
                                           "gamma"),
                   weights = "pred_deviation_log_gamma")
  
  cat(paste0(unique(x$person), "_" , 
             unique(x$course_Id), "_",
             "lognorm", " done\n"))
  
})




