paths <- read.table("paths_gamma_fused.txt", sep ="\t", stringsAsFactors = FALSE) 

x <- do.call(rbind, lapply(1:37, function(p) read.csv2(paste0(paths[p,],"/ErrorsPerJoint.csv"), 
                                           sep = "|")))[,-26]

errors_gamma <- colMeans(x)

paths <- read.table("paths_none_fused.txt", sep ="\t", stringsAsFactors = FALSE) 

x <- do.call(rbind, lapply(1:36, function(p) read.csv2(paste0(paths[p,],"/ErrorsPerJoint.csv"), 
                                                       sep = "|")))[,-26]

errors_none <- colMeans(x)


paths <- read.table("paths_boosting_fused.txt", sep ="\t", stringsAsFactors = FALSE) 

x <- do.call(rbind, lapply(1:36, function(p) read.csv2(paste0(paths[p,],"/ErrorsPerJoint.csv"), 
                                                       sep = "|")))[,-26]

errors_boosting <- colMeans(x)


data_errors <- data.frame(joint = names(errors_none), errors_none, errors_gamma, errors_boosting)

rownames(data_errors) <- NULL

save(data_errors, file = "../fusion_error.RData")
write.csv(data_errors, "../fusion_error.csv")
