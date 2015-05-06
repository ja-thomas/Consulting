library(MoCap)
paths <- read.table("../unregistered_paths.txt", sep = "\t", 
                    stringsAsFactors = FALSE)

result <- data.frame(path = paths[,1] ,cameras = rep(NA, nrow(paths)), 
                     pictures = rep(NA, nrow(paths)))


for(i in 1:nrow(paths)){
  d <- NULL
  tryCatch({d <- read_motion_data(paste0(".",paths[i,1]))}, 
           error = function(e) d <- NULL)
  
  if(!is.null(d)){
    result$cameras[i] <- (d$n_cameras - 1)
    result$pictures[i] <- (d$n_observations)
  }
  print(result[i,])
    
}

write.csv(result, "unregisterd_dimension.csv")
