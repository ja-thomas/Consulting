library(data.table)
setwd("~/Uni/Consulting/Code/")
load("../Data/data_processed.RData")

data[, azimut := acos(position_z/sqrt(position_x^2 + position_z^2))]

data[, elevation := acos(position_y/sqrt(position_y^2 + position_z^2))]



# ####################### Winkel der Peripherie
# 
# install.packages("ppls")
# library(ppls)
# 
# ### berechen auf vollständigen Daten data
# 
# 
# ###### Funktion zur Berechnung des Azimut: Peripheriewinkels mit x- und z- Koordinaten
# calc_azimut <- function(x){
# n <- nrow(x)
# azimut <- NULL
# for(i in 1:n){
#    
#    v <- acos(normalize.vector(c(data$position_x[i],data$position_z[i])) %*% normalize.vector(c(0,1)))
#     azimut <- c(azimut,v)
#   }
# return(azimut)
# }
# 
# # Datensatz aufteilen
# n <- nrow(data)
# azimut1 <- calc_azimut(data[1:1000000,])
# azimut2 <- calc_azimut(data[1000001:2000000,])
# azimut3 <- calc_azimut(data[2000001:3000000,])
# azimut4 <- calc_azimut(data[3000001:4000000,])
# azimut5 <- calc_azimut(data[4000001:5000000,])
# azimut6 <- calc_azimut(data[5000001:6000000,])
# azimut7 <- calc_azimut(data[6000001:7000000,])
# azimut8 <- calc_azimut(data[7000001:n,])
# # azimut <- calc_azimut(data) # testen
# 
# 
# ###### zusammenbauen von azimut
# azimut <- c(azimut1, azimut2, azimut3, azimut4, azimut5, azimut6, azimut7, azimut8)
# 
# rm(azimut1, azimut2, azimut3, azimut4, azimut5, azimut6,azimut7, azimut8)
# 
# ############## kompletter Datensatz mit azimut
# 
# data <- data.frame(data, azimut)
# 
# rm(azimut)
# 
# 
# 
# ###### Funktion zur Berechnung der Elevation: Höhenwinkel mit Horizont (Vektor (0,1)): Peripheriewinkel mit y und z
# calc_elevation<- function(x){
#   n <- nrow(x)
#   elevation <- NULL
#   for(i in 1:n){
#    v <- acos(normalize.vector(c(data$position_y[i],data$position_z[i])) %*% normalize.vector(c(0,1)))
#     elevation <- c(elevation,v)
#   }
#   return(elevation)
# }
# 
# n <- nrow(data)
# elevation1 <- calc_elevation(data[1:1000000,])
# elevation2 <- calc_elevation(data[1000001:2000000,])
# elevation3 <- calc_elevation(data[2000001:3000000,])
# elevation4 <- calc_elevation(data[3000001:4000000,])
# elevation5 <- calc_elevation(data[4000001:5000000,])
# elevation6 <- calc_elevation(data[5000001:6000000,])
# elevation7 <- calc_elevation(data[6000001:7000000,])
# elevation8 <- calc_elevation(data[7000001:n,])
# 
# #elevation <- calc_elevation(data) # testen
# 
# 
# ###### zusammenbauen von elevation
# elevation <- c(elevation1, elevation2, elevation3, elevation4, elevation5, elevation6, elevation7, elevation8)
# 
# rm(elevation1, elevation2, elevation3, elevation4, elevation5, elevation6, elevation7, elevation8)
# 
# ############## kompletter Datensatz mit elevation
# 
# data <- data.frame(data, elevation)
# 
# rm(elevation,n)
