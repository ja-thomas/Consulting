library(data.table)
library(reshape2)
library(devtools)
library(plyr)

load_all("MoCap/")

load("../Data/data.RData")

#remove Observations which have a deviation higher then 50 meters,
# all from Proband 3 Vor zurueck
data <- data[-which(data$deviation > 50),]



calculate_acceleration <- function(x){
  x <- x[with(x, order(timestamp)), ]
  x$position_change <- sqrt(c(NA, diff(x$position_x))^2 + 
                              c(NA, diff(x$position_y))^2 + 
                              c(NA, diff(x$position_z))^2)
  x$time_difference <- c(NA, diff(x$timestamp))/10000000
  x$acceleration <- (x$position_change / (x$time_difference))^2
  x
}



data <- ddply(data, ~joint_Nr+sensorId+person+course, calculate_acceleration)

data <- data.table(data)
setnames(data, "course", "course_Id")
data[, course := sub(" \\([123]\\)", "", course_Id)]

setkey(data, joint_Nr, course_Id, person, sensorId, timestamp)
summary(data)

save("data", file="data_processed.RData")


models <- dlply(data, ~joint_Nr, function(x)
  lm(deviation ~ kinect_error + camera_distance + acceleration + sensorId + person +course, data = x))

