####################### Winkel der Peripherie

install.packages("ppls")
library(ppls)

### berechen auf vollständigen Daten data


####### "Blickrichtung" des Sensors (0,-1) -> muss rotiert werden
## dazu Rotationsmatrix mit Vektor (0,0,1) multiplizieren: Ergebnisse 

# Kamera 501958741942
rot_x_501958741942 <- 0.748600220942998
rot_z_501958741942 <- 0.657482021077889

# Kamera 011921745247
rot_x_011921745247 <- -0.0371327856581523
rot_z_011921745247 <- 0.97213157607978 

# Kamera 500005441742
rot_x_500005441742 <- 0.0477760244965185
rot_z_500005441742 <- -0.984943693175208

# Kamera 500860243142
rot_x_500860243142 <- -0.728864778469335
rot_z_500860243142 <- 0.648697746109359

# Kamera 007319145247
rot_x_007319145247 <- -0.9804757965757
rot_z_007319145247 <- -0.0092022746072517

# Kamera 500718743142
rot_x_500718743142 <- -0.619461657018182
rot_z_500718743142 <- -0.743666820525266

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

rotations_camera <- data.frame(rot_x,rot_z)
rownames(rotations_camera) <- c("501958741942",
                         "011921745247",
                         "500005441742",
                         "500860243142",
                         "007319145247",
                         "500718743142")

rm(rot_x_501958741942,
   rot_x_011921745247,
   rot_x_500005441742,
   rot_x_500860243142,
   rot_x_007319145247,
   rot_x_500718743142,
   rot_z_501958741942,
   rot_z_011921745247,
   rot_z_500005441742,
   rot_z_500860243142,
   rot_z_007319145247,
   rot_z_500718743142)


############ Rotationsmatrizen der Sensoren

rotmat_501958741942 <- t(matrix(c(-0.659233154387982, -0.0707768137158421, -0.748600220942998,
                                  -0.00984094353138229, 0.996287134648934, -0.0855283646717836,
                                  0.751874194254653, -0.0490162011303444, -0.657482021077889),
                                nrow = 3, ncol =3))



rotmat_011921745247 <-t(matrix(c( -0.999270240386041, -0.00895225715055656, 0.0371327856581523,
                                 -0.0173157410574382, 0.972686388356351, -0.231476467525122,
                                 -0.0340463183098517, -0.231950527048738, -0.97213157607978),
                              nrow = 3, ncol = 3))


rotmat_500005441742 <- t(matrix(c(0.998356967561025, -0.0316356887969959, -0.0477760244965185,
                                  0.0232879355395554, 0.985826708555117, -0.166142627756063,
                                  0.0523549174448575, 0.164757045050352, 0.984943693175208),
                                nrow = 3, ncol = 3))


rotmat_500860243142 <- t(matrix(c(-0.666867760999647, 0.155059743473815, 0.728864778469335,
                                  -0.0124677253813732, 0.975652185424872, -0.218968876554374,
                                  -0.745071771820093, -0.155110570334535, -0.648697746109359),
                                nrow = 3, ncol = 3))

rotmat_007319145247 <- t(matrix(c( 0.00421302862283504, 0.196594666049388, 0.9804757965757,
                                   -0.0258030943163512, 0.980179406972214, -0.196424363232517,
                                   -0.999658166931839, -0.0244717679894217, 0.0092022746072517),
                                nrow = 3, ncol = 3))


rotmat_500718743142 <- t(matrix(c(0.767589384408815, 0.16454115724397, 0.619461657018182,
                                  -0.00454585949920858, 0.967859710716012, -0.251449628224284,
                                  -0.640925792977868, 0.190194079680552, 0.743666820525266),
                                nrow = 3, ncol = 3))

################# Berechnung des Peripherie-Winkels
#install.packages("ppls")
library(ppls)
data <- data2[5000001:6000000,]

n <- nrow(data)
periphery <- NULL
for(i in 1:n){
  vec_joint <-  - c(data$position_x[i], data$position_y[i],data$position_z[i])
  
  if(data$sensorId[i] == "501958741942"){
    vec_joint <- rotmat_501958741942%*%vec_joint
    vec_joint <- c(vec_joint[1], vec_joint[3])
    v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[1,])))
    periphery <- c(periphery,v)
  }
  else{
    if(data$sensorId[i] == "011921745247"){
      vec_joint <- rotmat_011921745247%*%vec_joint
      vec_joint <- c(vec_joint[1], vec_joint[3])
      v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[2,])))
      periphery <- c(periphery,v)
    }
    else{
      if(data$sensorId[i] == "500005441742"){
        vec_joint <- rotmat_500005441742%*%vec_joint
        vec_joint <- c(vec_joint[1], vec_joint[3])
        v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[3,])))
        periphery <- c(periphery,v)
      }
      else{
        if(data$sensorId[i] == "500860243142"){
          vec_joint <- rotmat_500860243142*vec_joint
          vec_joint <- c(vec_joint[1], vec_joint[3])
          v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[4,])))
          periphery <- c(periphery,v)
        }
        else{
          if(data$sensorId[i] == "007319145247"){
            vec_joint <- rotmat_007319145247 %*% vec_joint
            vec_joint <- c(vec_joint[1], vec_joint[3])
            v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[5,])))
            periphery <- c(periphery,v)
          }
          else{
            if(data$sensorId[i] == "500718743142"){
              vec_joint <- rotmat_500718743142%*%vec_joint
              vec_joint <- c(vec_joint[1], vec_joint[3])
              v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[6,])))
              periphery <- c(periphery,v)
            }
            
          }
        }
        
      }
      
    }
    
  }
  
}

periphery5 <- periphery


#############################

data <- data2[6000001:7000000,]

n <- nrow(data)
periphery <- NULL
for(i in 1:n){
  vec_joint <-  - c(data$position_x[i], data$position_y[i],data$position_z[i])
  
  if(data$sensorId[i] == "501958741942"){
    vec_joint <- rotmat_501958741942%*%vec_joint
    vec_joint <- c(vec_joint[1], vec_joint[3])
    v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[1,])))
    periphery <- c(periphery,v)
  }
  else{
    if(data$sensorId[i] == "011921745247"){
      vec_joint <- rotmat_011921745247%*%vec_joint
      vec_joint <- c(vec_joint[1], vec_joint[3])
      v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[2,])))
      periphery <- c(periphery,v)
    }
    else{
      if(data$sensorId[i] == "500005441742"){
        vec_joint <- rotmat_500005441742%*%vec_joint
        vec_joint <- c(vec_joint[1], vec_joint[3])
        v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[3,])))
        periphery <- c(periphery,v)
      }
      else{
        if(data$sensorId[i] == "500860243142"){
          vec_joint <- rotmat_500860243142*vec_joint
          vec_joint <- c(vec_joint[1], vec_joint[3])
          v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[4,])))
          periphery <- c(periphery,v)
        }
        else{
          if(data$sensorId[i] == "007319145247"){
            vec_joint <- rotmat_007319145247 %*% vec_joint
            vec_joint <- c(vec_joint[1], vec_joint[3])
            v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[5,])))
            periphery <- c(periphery,v)
          }
          else{
            if(data$sensorId[i] == "500718743142"){
              vec_joint <- rotmat_500718743142%*%vec_joint
              vec_joint <- c(vec_joint[1], vec_joint[3])
              v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[6,])))
              periphery <- c(periphery,v)
            }
            
          }
        }
        
      }
      
    }
    
  }
  
}

periphery6 <- periphery

data <- data2[7000001:8000000,]

n <- nrow(data)
periphery <- NULL
for(i in 1:n){
  vec_joint <-  - c(data$position_x[i], data$position_y[i],data$position_z[i])
  
  if(data$sensorId[i] == "501958741942"){
    vec_joint <- rotmat_501958741942%*%vec_joint
    vec_joint <- c(vec_joint[1], vec_joint[3])
    v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[1,])))
    periphery <- c(periphery,v)
  }
  else{
    if(data$sensorId[i] == "011921745247"){
      vec_joint <- rotmat_011921745247%*%vec_joint
      vec_joint <- c(vec_joint[1], vec_joint[3])
      v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[2,])))
      periphery <- c(periphery,v)
    }
    else{
      if(data$sensorId[i] == "500005441742"){
        vec_joint <- rotmat_500005441742%*%vec_joint
        vec_joint <- c(vec_joint[1], vec_joint[3])
        v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[3,])))
        periphery <- c(periphery,v)
      }
      else{
        if(data$sensorId[i] == "500860243142"){
          vec_joint <- rotmat_500860243142*vec_joint
          vec_joint <- c(vec_joint[1], vec_joint[3])
          v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[4,])))
          periphery <- c(periphery,v)
        }
        else{
          if(data$sensorId[i] == "007319145247"){
            vec_joint <- rotmat_007319145247 %*% vec_joint
            vec_joint <- c(vec_joint[1], vec_joint[3])
            v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[5,])))
            periphery <- c(periphery,v)
          }
          else{
            if(data$sensorId[i] == "500718743142"){
              vec_joint <- rotmat_500718743142%*%vec_joint
              vec_joint <- c(vec_joint[1], vec_joint[3])
              v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[6,])))
              periphery <- c(periphery,v)
            }
            
          }
        }
        
      }
      
    }
    
  }
  
}

periphery7 <- periphery



data <- data2[8000001:9086443,]

n <- nrow(data)
periphery <- NULL
for(i in 1:n){
  vec_joint <-  - c(data$position_x[i], data$position_y[i],data$position_z[i])
  
  if(data$sensorId[i] == "501958741942"){
    vec_joint <- rotmat_501958741942%*%vec_joint
    vec_joint <- c(vec_joint[1], vec_joint[3])
    v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[1,])))
    periphery <- c(periphery,v)
  }
  else{
    if(data$sensorId[i] == "011921745247"){
      vec_joint <- rotmat_011921745247%*%vec_joint
      vec_joint <- c(vec_joint[1], vec_joint[3])
      v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[2,])))
      periphery <- c(periphery,v)
    }
    else{
      if(data$sensorId[i] == "500005441742"){
        vec_joint <- rotmat_500005441742%*%vec_joint
        vec_joint <- c(vec_joint[1], vec_joint[3])
        v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[3,])))
        periphery <- c(periphery,v)
      }
      else{
        if(data$sensorId[i] == "500860243142"){
          vec_joint <- rotmat_500860243142*vec_joint
          vec_joint <- c(vec_joint[1], vec_joint[3])
          v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[4,])))
          periphery <- c(periphery,v)
        }
        else{
          if(data$sensorId[i] == "007319145247"){
            vec_joint <- rotmat_007319145247 %*% vec_joint
            vec_joint <- c(vec_joint[1], vec_joint[3])
            v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[5,])))
            periphery <- c(periphery,v)
          }
          else{
            if(data$sensorId[i] == "500718743142"){
              vec_joint <- rotmat_500718743142%*%vec_joint
              vec_joint <- c(vec_joint[1], vec_joint[3])
              v <- acos(normalize.vector(vec_joint) %*% normalize.vector(as.numeric(rotations_camera[6,])))
              periphery <- c(periphery,v)
            }
            
          }
        }
        
      }
      
    }
    
  }
  
}

periphery8 <- periphery



###### zusammenbauen von periphery
periphery <- c(periphery1, periphery2, periphery3, periphery4, periphery5, periphery6, periphery7, periphery8)

rm(periphery1, periphery2, periphery3, periphery4, periphery5, periphery6, periphery7, periphery8)

data <- data2
rm(data2)
rm(rotations_camera, v, i, n, rot_x, rot_z, vec_joint)


periphery_data <- data.frame(data[,c(1,2,3,4,5)], periphery)

rm(data, periphery)
