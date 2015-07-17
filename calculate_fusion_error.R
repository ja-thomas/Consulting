paths <- read.table("paths_gamma_fused.txt", sep ="\t", stringsAsFactors = FALSE) 

x <- do.call(rbind, lapply(1:37, function(p) read.csv2(paste0(paths[p,],"/ErrorsPerJoint.csv"), 
                                           sep = "|")))[,-26]

errors_gamma <- colMeans(x)

paths <- read.table("paths_none_fused.txt", sep ="\t", stringsAsFactors = FALSE) 

x <- do.call(rbind, lapply(1:36, function(p) read.csv2(paste0(paths[p,],"/ErrorsPerJoint.csv"), 
                                                       sep = "|")))[,-26]

errors_none <- colMeans(x)


data_errors <- data.frame(joint = names(errors_none), errors_none, errors_gamma)

rownames(data_errors) <- NULL

data_errors$difference = data_errors$errors_gamma - data_errors$errors_none
