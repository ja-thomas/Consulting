####################### Winkel der Peripherie

install.packages("ppls")
library(ppls)

### berechen auf vollständigen Daten data


###### Funktion zur Berechnung des Azimut: Peripheriewinkels mit x- und z- Koordinaten
calc_azimut <- function(x){
n <- nrow(x)
azimut <- NULL
for(i in 1:n){
   
   v <- acos(normalize.vector(c(data$position_x[i],data$position_z[i])) %*% normalize.vector(c(0,1)))
    azimut <- c(azimut,v)
  }
return(azimut)
}

# Datensatz aufteilen
azimut1 <- calc_azimut(data[person == "Proband 1",])
azimut2 <- calc_azimut(data[person == "Proband 2",])
azimut3 <- calc_azimut(data[person == "Proband 3",])
azimut4 <- calc_azimut(data[person == "Proband 4",])
azimut5 <- calc_azimut(data[person == "Proband 5",])

# azimut <- calc_azimut(data) # testen


###### zusammenbauen von azimut
azimut <- c(azimut1, azimut2, azimut3, azimut4, azimut5)

rm(azimut1, azimut2, azimut3, azimut4, azimut5)

############## kompletter Datensatz mit azimut

data <- data.frame(data, azimut)

rm(azimut)



###### Funktion zur Berechnung der Elevation: Höhenwinkel mit Horizont (Vektor (0,1)): Peripheriewinkel mit y und z
calc_elevation<- function(x){
  n <- nrow(x)
  elevation <- NULL
  for(i in 1:n){
   v <- acos(normalize.vector(c(data$position_y[i],data$position_z[i])) %*% normalize.vector(c(0,1)))
    elevation <- c(elevation,v)
  }
  return(elevation)
}

elevation1 <- calc_elevation(data[person == "Proband 1",])
elevation2 <- calc_elevation(data[person == "Proband 2",]) 
elevation3 <- calc_elevation(data[person == "Proband 3",])
elevation4 <- calc_elevation(data[person == "Proband 4",])
elevation5 <- calc_elevation(data[person == "Proband 5",])

#elevation <- calc_elevation(data) # testen


###### zusammenbauen von elevation
elevation <- c(elevation1, elevation2, elevation3, elevation4, elevation5)

rm(elevation1, elevation2, elevation3, elevation4, elevation5)

############## kompletter Datensatz mit elevation

data <- data.frame(data, elevation)

rm(elevation)
