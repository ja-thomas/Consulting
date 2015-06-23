library(MoCap)
library(data.table)
library(plyr)
setwd("Uni/Consulting/Code/")

load("../Data/data_processed.RData")

data[, acceleration_diff := c(NA, diff(acceleration))]


library(forecast)

x <- auto.arima(data[.(1, "Arm kreisen", "Proband 4", "007319145247"), position_x])


unique(data$joint_Nr), unique(data$sensorId), unique(data$person), unique(data$course_ID)


data <- ddply(data, ~joint_Nr+sensorId+person+course, forecast_positions)



forecast_positions <- function(data){
  data$forecast_x <- abs(auto.arima(data$position_x)$residuals)
  data$forecast_y <- abs(auto.arima(data$position_y)$residuals)
  data$forecast_z <- abs(auto.arima(data$position_z)$residuals)
  data
}