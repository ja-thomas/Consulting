###################### Variable "angle_shoulder" erstellen

install.packages("devtools")
library(devtools)


install_github("ja-thomas/MoCap")


library(MoCap)


################################################################


joint_numbers # Anzeigen der Joints -> Shoulder Left = 4 und Shoulder Right = 8

angle_data_4 <- work_data[(work_data$joint_Nr == 4),]
angle_data_8 <- work_data[(work_data$joint_Nr == 8),]

attach(angle_data_4)
angle_data_4 <- data.frame( timestamp , sensorId, person,
                         course_Id, position_x, position_z)
colnames(angle_data_4) <- c("timestamp", "sensorId", "person",
                         "course_Id", "LShoulder_position_x", "LShoulder_position_z")

detach(angle_data_4)

attach(angle_data_8)
angle_data_8 <- data.frame(timestamp, sensorId, person,
                          course_Id, position_x, position_z)
colnames(angle_data_8) <- c("timestamp", "sensorId", "person",
                         "course_Id", "RShoulder_position_x", "RShoulder_position_z")

detach(angle_data_8)

############# Datensatz umbauen

angle_data <- merge(angle_data_8, angle_data_4, all = FALSE)
rm(angle_data_4)
rm(angle_data_8)
############# Subtrahieren der Positionsdaten: Shoulder Right (8) - Shoulder Left (4)
attach(angle_data)

# x-Wert
diff_x <- RShoulder_position_x - LShoulder_position_x

#z-Wert
diff_z <- RShoulder_position_z - LShoulder_position_z

# Abstandsvektor ist (diff_x, diff_z)

##### Rotatieren um 90 Grad

# rotationsmatrix erstellen

M <- matrix(c(0,-1,1,0),2) # Rotieren durch M* (diff_x,diff_z) -> (-diff_x, diff_z)

rot_diff_x <- -diff_z
rot_diff_z <- diff_x

rm(M)
### Zusammenfügen zu angle_data

angle_data <- data.frame(angle_data, rot_diff_x, rot_diff_z)
rm(diff_x)
rm(diff_z)
rm(rot_diff_x)
rm(rot_diff_y)


####### Blickrichtung des Sensors
## dazu Rotationsmatrix mit Vektor (0,0,1) multiplizieren: Ergebnisse 

# Kamera 501958741942
rot_x_501958741942 <- -0.748600220942998
rot_z_501958741942 <- -0.657482021077889

# Kamera 011921745247
rot_x_011921745247 <- 0.0371327856581523
rot_z_011921745247 <- -0.97213157607978 

# Kamera 500005441742
rot_x_500005441742 <- -0.0477760244965185
rot_z_500005441742 <- 0.984943693175208

# Kamera 500860243142
rot_x_500860243142 <- 0.728864778469335
rot_z_500860243142 <- -0.648697746109359

# Kamera 007319145247
rot_x_007319145247 <- 0.9804757965757
rot_z_007319145247 <- 0.0092022746072517

# Kamera 500718743142
rot_x_500718743142 <- 0.619461657018182
rot_z_500718743142 <- 0.743666820525266

rot_x <- c(rot_x_501958741942,
           rot_x_011921745247,
           rot_x_500005441742,
           rot_x_500860243142,
           rot_x_007319145247,
           rot_x_500718743142)
rot_z <- c(rot_z_501958741942,
           rot_z_011921745247,
           rot_z_500005441742,
           rot_z_500860243142,
           rot_z_007319145247,
           rot_z_500718743142)

rotations <- data.frame(rot_x,rot_z)
rownames(rotations) <- c("501958741942",
                         "011921745247",
                         "500005441742",
                         "500860243142",
                         "007319145247",
                         "500718743142")


# Variable rot_cam_x erstellen
n <- nrow(angle_data)
rot_cam_x <- NULL
for(i in 1:n){
  if(angle_data$sensorId[i] == "501958741942"){
    rot_cam_x <- c(rot_cam_x, rotations[1,1])
  }
  else {
    
    if(angle_data$sensorId[i] == "011921745247"){
      rot_cam_x <- c(rot_cam_x, rotations[2,1])
  }
  else{
    
    if(angle_data$sensorId[i] == "500005441742"){
      rot_cam_x <- c(rot_cam_x, rotations[3,1])
    }
    else{
      
      if(angle_data$sensorId[i] == "500860243142"){
        rot_cam_x <- c(rot_cam_x, rotations[4,1])
      }
      else{
        
        if(angle_data$sensorId[i] == "007319145247"){
          rot_cam_x <- c(rot_cam_x, rotations[5,1])
        }
        else{
          
          if(angle_data$sensorId[i] == "500718743142"){
            rot_cam_x <- c(rot_cam_x, rotations[6,1])
        }
      }
      }  
    }
  }
  } 
  }
  
}

# Variable rot_cam_z erstellen
n <- nrow(angle_data)
rot_cam_z <- NULL
for(i in 1:n){
  if(angle_data$sensorId[i] == "501958741942"){
    rot_cam_z <- c(rot_cam_z, rotations[1,2])
  }
  else {
    
    if(angle_data$sensorId[i] == "011921745247"){
      rot_cam_z <- c(rot_cam_z, rotations[2,2])
    }
    else{
      
      if(angle_data$sensorId[i] == "500005441742"){
        rot_cam_z <- c(rot_cam_z, rotations[3,2])
      }
      else{
        
        if(angle_data$sensorId[i] == "500860243142"){
          rot_cam_z <- c(rot_cam_z, rotations[4,2])
        }
        else{
          
          if(angle_data$sensorId[i] == "007319145247"){
            rot_cam_z <- c(rot_cam_z, rotations[5,2])
          }
          else{
            
            if(angle_data$sensorId[i] == "500718743142"){
              rot_cam_z <- c(rot_cam_z, rotations[6,2])
            }
          }
        }  
      }
    }
  } 
}

}



############# Vektoren normalisieren

install.packages("ppls")
library(ppls)

### für Blickrichtung des Nutzers
norm_vec_user_x <- NULL
norm_vec_user_z <- NULL
for(i in 1:n){
  a <- normalize.vector(c(angle_data$diff_x[i],angle_data$diff_z[i]))
  norm_vec_user_x <- c(norm_vec_user_x,a[1])
  norm_vec_user_z <- c(norm_vec_user_z,a[2])
}

### für Blickrichtung der Kamera
norm_vec_cam_x <- NULL
norm_vec_cam_z <- NULL
for(i in 1:n){
  a <- normalize.vector(c(rot_cam_x[i],rot_cam_z[i]))
  norm_vec_cam_x <- c(norm_vec_cam_x,a[1])
  norm_vec_cam_z <- c(norm_vec_cam_z,a[2])
}



######## Punktprodukt berechnen
shoulder_angle <- NULL
for(i in 1:n){
  v <- acos(c(norm_vec_user_x[i], norm_vec_user_z[i]) %*% c(norm_vec_cam_x[i], norm_vec_cam_z[i]))
  shoulder_angle <- c(shoulder_angle,v)
}

####### Datensatz mit Identifikationsvariablen und Winkelvariable als Ergebnis

angle_data_red <- angle_data[,c(1,2,3,4)]
angle_data_red <- cbind(angle_data_red,shoulder_angle)


rm(list=(ls()[ls()!= "angle_data_red"]))


