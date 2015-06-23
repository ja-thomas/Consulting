########## Variable für Knochenlänge

# interessierende Variablen aus Gesamtdatensatz herauspicken
attach(data)
bone_data <- data.frame(timestamp, joint_Nr, sensorId, person, course_Id, position_x,
                        position_y, position_z)
detach(data)
rm(data)

##### Datensatz umstrukturieren



#### für jeden Joint eigenen Datensatz und eigenen Benennung der x,y,z-Positionsvariablen
for(i in 0:24){
  
  a <- bone_data[(bone_data$joint_Nr == i),]
  
  # x,y,z Koordinaten identifizierbar machen
  names(a)[names(a)=="position_x"] <- paste0("x",i)
  names(a)[names(a)=="position_y"] <- paste0("y",i)
  names(a)[names(a)=="position_z"] <- paste0("z",i)
  
   a <- a[,-2] # Variable Joint Nr entfernen 
  # Datensätze durchnummerieren
  assign(paste0("joint",i),a)
}

rm(a)
# neuer Datensatz mit allen Positionsdaten pro Zeitpunkt, Kamera, etc.
bone_data <- Reduce(function(x, y) merge(x, y,
                                 by = c("timestamp", "sensorId", "person", "course_Id"),
                                 all=TRUE), list(joint0,joint1, joint2, joint3, joint4, joint5,
                                                  joint6, joint7, joint8, joint9, joint10,
                                                  joint11, joint12, joint13, joint14, joint15,
                                                  joint16, joint17, joint18, joint19, joint20,
                                                  joint21, joint22, joint23, joint24))
rm(joint0,joint1, joint2, joint3, joint4, joint5,
   joint6, joint7, joint8, joint9, joint10,
   joint11, joint12, joint13, joint14, joint15,
   joint16, joint17, joint18, joint19, joint20,
   joint21, joint22, joint23, joint24)

######## Datensatz mit x-Werten, y-Werten, z-Werten
attach(bone_data)
xdata <- data.frame(timestamp, sensorId, person, course_Id,x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24)
ydata <- data.frame(timestamp, sensorId, person, course_Id,y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21,y22,y23,y24)
zdata <- data.frame(timestamp, sensorId, person, course_Id,z0,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15,z16,z17,z18,z19,z20,z21,z22,z23,z24)
detach(bone_data)

### kinematisches Modell geordnet nach Jointnummerierung
Jointmatrix_sortiert <- read.csv("~/Consulting/Jointmatrix_sortiert.csv", sep=";")
jointmat <- Jointmatrix_sortiert[,-1] #25x25

#################################### Berechnen der Knochenlängenvariable


cbind.fill <- function(...){     # Quelle: http://stackoverflow.com/questions/7962267/cbind-a-df-with-an-empty-df-cbind-fill
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

########## nur für Proband 1 
xdata2 <- xdata
ydata2 <- ydata
zdata2 <- zdata

xdata <- xdata2[xdata$person == "Proband 1",]
ydata <- ydata2[ydata$person == "Proband 1",]
zdata <- zdata2[zdata$person == "Proband 1",]

##### Funktion zur Berechnung der Knochenlängenvariable

n <- nrow(xdata)

for(l in 1:25){ # Zeilendurchlauf
  mat_diff <- vector()
  for(m in 1:25){ # Spaltendurchlauf
  
 if(jointmat[l,m] == 1){ # falls Werte[l,m] 1 ist, sind Joints mit Nummer l-1 und m-1 verbunden
   

a <- NULL
for(i in 1:n){ # Abstand der beiden Joints berechnen
  abst <- sqrt((xdata[i,l+4]-xdata[i,m+4])^2 + (ydata[i,l+4]-ydata[i,m+4])^2 + (zdata[i,l+4]-zdata[i,m+4])^2)
  a <- c(a,abst) #Abstandsdaten werden in Variable a gespeichert
}

# Knochenlängenvariable ist skalierte Differenz zwischen Abstand der beiden Joints und dessen Median -> skalierter Fehler
mat_diff <- cbind.fill(mat_diff, scale(abs(a - median(a)), center = FALSE)) # Matrix wird aufgebaut für jeden verbundenen Joint eine Spalte

} # Ende if-Abfrage
} # Ende Spaltendurchlauf



j <- ncol(mat_diff)
if( j == 2){
  mat_diff <- mat_diff[,-1]
 bone_error <- mat_diff # bei einem angrenzenden Joint -> Wert übernehmen
}
else{
  if(j == 3){
   mat_diff <- mat_diff[,-1]
  bone_error <- NULL
  for(k in 1:n){
  smallest <- min(mat_diff[k,])
  bone_error <- c(bone_error,smallest)
  }
}
else{
  mat_diff <- mat_diff[,-1]
  bone_error <- NULL
for(k in 1:n){
  sorted <- sort(mat_diff[k,])
  bone_error <- c(bone_error,sorted[2])
  } 
}
}# Ende else

assign(paste0("Prob1Knochen_Joint",l-1),data.frame(xdata[,c(1,2,3,4)],bone_error)) 

}






########## nur für Proband 2 

xdata <- xdata2[xdata2$person == "Proband 2",]
ydata <- ydata2[ydata2$person == "Proband 2",]
zdata <- zdata2[zdata2$person == "Proband 2",]

##### Funktion zur Berechnung der Knochenlängenvariable

n <- nrow(xdata)

for(l in 1:25){ # Zeilendurchlauf
  mat_diff <- vector()
  for(m in 1:25){ # Spaltendurchlauf
    
    if(jointmat[l,m] == 1){ # falls Werte[l,m] 1 ist, sind Joints mit Nummer l-1 und m-1 verbunden
      
      
      a <- NULL
      for(i in 1:n){ # Abstand der beiden Joints berechnen
        abst <- sqrt((xdata[i,l+4]-xdata[i,m+4])^2 + (ydata[i,l+4]-ydata[i,m+4])^2 + (zdata[i,l+4]-zdata[i,m+4])^2)
        a <- c(a,abst) #Abstandsdaten werden in Variable a gespeichert
      }
      
      # Knochenlängenvariable ist skalierte Differenz zwischen Abstand der beiden Joints und dessen Median -> skalierter Fehler
      mat_diff <- cbind.fill(mat_diff, scale(abs(a - median(a)), center = FALSE)) # Matrix wird aufgebaut für jeden verbundenen Joint eine Spalte
      
    } # Ende if-Abfrage
  } # Ende Spaltendurchlauf
  
  
  
  j <- ncol(mat_diff)
  if( j == 2){
    mat_diff <- mat_diff[,-1]
    bone_error <- mat_diff # bei einem angrenzenden Joint -> Wert übernehmen
  }
  else{
    if(j == 3){
      mat_diff <- mat_diff[,-1]
      bone_error <- NULL
      for(k in 1:n){
        smallest <- min(mat_diff[k,])
        bone_error <- c(bone_error,smallest)
      }
    }
    else{
      mat_diff <- mat_diff[,-1]
      bone_error <- NULL
      for(k in 1:n){
        sorted <- sort(mat_diff[k,])
        bone_error <- c(bone_error,sorted[2])
      } 
    }
  }# Ende else
  
  assign(paste0("Prob2Knochen_Joint",l-1),data.frame(xdata[,c(1,2,3,4)],bone_error)) 
  
}

########## nur für Proband 3 

xdata <- xdata2[xdata2$person == "Proband 3",]
ydata <- ydata2[ydata2$person == "Proband 3",]
zdata <- zdata2[zdata2$person == "Proband 3",]

##### Funktion zur Berechnung der Knochenlängenvariable

n <- nrow(xdata)

for(l in 1:25){ # Zeilendurchlauf
  mat_diff <- vector()
  for(m in 1:25){ # Spaltendurchlauf
    
    if(jointmat[l,m] == 1){ # falls Werte[l,m] 1 ist, sind Joints mit Nummer l-1 und m-1 verbunden
      
      
      a <- NULL
      for(i in 1:n){ # Abstand der beiden Joints berechnen
        abst <- sqrt((xdata[i,l+4]-xdata[i,m+4])^2 + (ydata[i,l+4]-ydata[i,m+4])^2 + (zdata[i,l+4]-zdata[i,m+4])^2)
        a <- c(a,abst) #Abstandsdaten werden in Variable a gespeichert
      }
      
      # Knochenlängenvariable ist skalierte Differenz zwischen Abstand der beiden Joints und dessen Median -> skalierter Fehler
      mat_diff <- cbind.fill(mat_diff, scale(abs(a - median(a)), center = FALSE)) # Matrix wird aufgebaut für jeden verbundenen Joint eine Spalte
      
    } # Ende if-Abfrage
  } # Ende Spaltendurchlauf
  
  
  
  j <- ncol(mat_diff)
  if( j == 2){
    mat_diff <- mat_diff[,-1]
    bone_error <- mat_diff # bei einem angrenzenden Joint -> Wert übernehmen
  }
  else{
    if(j == 3){
      mat_diff <- mat_diff[,-1]
      bone_error <- NULL
      for(k in 1:n){
        smallest <- min(mat_diff[k,])
        bone_error <- c(bone_error,smallest)
      }
    }
    else{
      mat_diff <- mat_diff[,-1]
      bone_error <- NULL
      for(k in 1:n){
        sorted <- sort(mat_diff[k,])
        bone_error <- c(bone_error,sorted[2])
      } 
    }
  }# Ende else
  
  assign(paste0("Prob3Knochen_Joint",l-1),data.frame(xdata[,c(1,2,3,4)],bone_error)) 
  
}

##################################### nur für Proband 4

xdata <- xdata2[xdata2$person == "Proband 4",]
ydata <- ydata2[ydata2$person == "Proband 4",]
zdata <- zdata2[zdata2$person == "Proband 4",]

##### Funktion zur Berechnung der Knochenlängenvariable

n <- nrow(xdata)

for(l in 1:25){ # Zeilendurchlauf
  mat_diff <- vector()
  for(m in 1:25){ # Spaltendurchlauf
    
    if(jointmat[l,m] == 1){ # falls Werte[l,m] 1 ist, sind Joints mit Nummer l-1 und m-1 verbunden
      
      
      a <- NULL
      for(i in 1:n){ # Abstand der beiden Joints berechnen
        abst <- sqrt((xdata[i,l+4]-xdata[i,m+4])^2 + (ydata[i,l+4]-ydata[i,m+4])^2 + (zdata[i,l+4]-zdata[i,m+4])^2)
        a <- c(a,abst) #Abstandsdaten werden in Variable a gespeichert
      }
      
      # Knochenlängenvariable ist skalierte Differenz zwischen Abstand der beiden Joints und dessen Median -> skalierter Fehler
      mat_diff <- cbind.fill(mat_diff, scale(abs(a - median(a)), center = FALSE)) # Matrix wird aufgebaut für jeden verbundenen Joint eine Spalte
      
    } # Ende if-Abfrage
  } # Ende Spaltendurchlauf
  
  
  
  j <- ncol(mat_diff)
  if( j == 2){
    mat_diff <- mat_diff[,-1]
    bone_error <- mat_diff # bei einem angrenzenden Joint -> Wert übernehmen
  }
  else{
    if(j == 3){
      mat_diff <- mat_diff[,-1]
      bone_error <- NULL
      for(k in 1:n){
        smallest <- min(mat_diff[k,])
        bone_error <- c(bone_error,smallest)
      }
    }
    else{
      mat_diff <- mat_diff[,-1]
      bone_error <- NULL
      for(k in 1:n){
        sorted <- sort(mat_diff[k,])
        bone_error <- c(bone_error,sorted[2])
      } 
    }
  }# Ende else
  
  assign(paste0("Prob4Knochen_Joint",l-1),data.frame(xdata[,c(1,2,3,4)],bone_error)) 
  
}

########## nur für Proband 5

xdata <- xdata2[xdata2$person == "Proband 5",]
ydata <- ydata2[ydata2$person == "Proband 5",]
zdata <- zdata2[zdata2$person == "Proband 5",]

##### Funktion zur Berechnung der Knochenlängenvariable

n <- nrow(xdata)

for(l in 1:25){ # Zeilendurchlauf
  mat_diff <- vector()
  for(m in 1:25){ # Spaltendurchlauf
    
    if(jointmat[l,m] == 1){ # falls Werte[l,m] 1 ist, sind Joints mit Nummer l-1 und m-1 verbunden
      
      
      a <- NULL
      for(i in 1:n){ # Abstand der beiden Joints berechnen
        abst <- sqrt((xdata[i,l+4]-xdata[i,m+4])^2 + (ydata[i,l+4]-ydata[i,m+4])^2 + (zdata[i,l+4]-zdata[i,m+4])^2)
        a <- c(a,abst) #Abstandsdaten werden in Variable a gespeichert
      }
      
      # Knochenlängenvariable ist skalierte Differenz zwischen Abstand der beiden Joints und dessen Median -> skalierter Fehler
      mat_diff <- cbind.fill(mat_diff, scale(abs(a - median(a)), center = FALSE)) # Matrix wird aufgebaut für jeden verbundenen Joint eine Spalte
      
    } # Ende if-Abfrage
  } # Ende Spaltendurchlauf
  
  
  
  j <- ncol(mat_diff)
  if( j == 2){
    mat_diff <- mat_diff[,-1]
    bone_error <- mat_diff # bei einem angrenzenden Joint -> Wert übernehmen
  }
  else{
    if(j == 3){
      mat_diff <- mat_diff[,-1]
      bone_error <- NULL
      for(k in 1:n){
        smallest <- min(mat_diff[k,])
        bone_error <- c(bone_error,smallest)
      }
    }
    else{
      mat_diff <- mat_diff[,-1]
      bone_error <- NULL
      for(k in 1:n){
        sorted <- sort(mat_diff[k,])
        bone_error <- c(bone_error,sorted[2])
      } 
    }
  }# Ende else
  
  assign(paste0("Prob5Knochen_Joint",l-1),data.frame(xdata[,c(1,2,3,4)],bone_error)) 
  
}


rm(xdata, ydata, zdata, xdata2, ydata2, zdata2)

# Proband 1 Datensätze zusammenfügen 
n <- nrow(Prob1Knochen_Joint0)

joint_Nr <- rep(0,n)
Prob1Knochen_Joint0 <- data.frame(Prob1Knochen_Joint0, joint_Nr)
joint_Nr <- rep(1,n)
Prob1Knochen_Joint1 <- data.frame(Prob1Knochen_Joint1, joint_Nr)
joint_Nr <- rep(2,n)
Prob1Knochen_Joint2 <- data.frame(Prob1Knochen_Joint2, joint_Nr)
joint_Nr <- rep(3,n)
Prob1Knochen_Joint3 <- data.frame(Prob1Knochen_Joint3, joint_Nr)
joint_Nr <- rep(4,n)
Prob1Knochen_Joint4 <- data.frame(Prob1Knochen_Joint4, joint_Nr)
joint_Nr <- rep(5,n)
Prob1Knochen_Joint5 <- data.frame(Prob1Knochen_Joint5, joint_Nr)
joint_Nr <- rep(6,n)
Prob1Knochen_Joint6 <- data.frame(Prob1Knochen_Joint6, joint_Nr)
joint_Nr <- rep(7,n)
Prob1Knochen_Joint7 <- data.frame(Prob1Knochen_Joint7, joint_Nr)
joint_Nr <- rep(8,n)
Prob1Knochen_Joint8 <- data.frame(Prob1Knochen_Joint8, joint_Nr)
joint_Nr <- rep(9,n)
Prob1Knochen_Joint9 <- data.frame(Prob1Knochen_Joint9, joint_Nr)
joint_Nr <- rep(10,n)
Prob1Knochen_Joint10 <- data.frame(Prob1Knochen_Joint10, joint_Nr)
joint_Nr <- rep(11,n)
Prob1Knochen_Joint11 <- data.frame(Prob1Knochen_Joint11, joint_Nr)
joint_Nr <- rep(12,n)
Prob1Knochen_Joint12 <- data.frame(Prob1Knochen_Joint12, joint_Nr)
joint_Nr <- rep(13,n)
Prob1Knochen_Joint13 <- data.frame(Prob1Knochen_Joint13, joint_Nr)
joint_Nr <- rep(14,n)
Prob1Knochen_Joint14 <- data.frame(Prob1Knochen_Joint14, joint_Nr)
joint_Nr <- rep(15,n)
Prob1Knochen_Joint15 <- data.frame(Prob1Knochen_Joint15, joint_Nr)
joint_Nr <- rep(16,n)
Prob1Knochen_Joint16 <- data.frame(Prob1Knochen_Joint16, joint_Nr)
joint_Nr <- rep(17,n)
Prob1Knochen_Joint17 <- data.frame(Prob1Knochen_Joint17, joint_Nr)
joint_Nr <- rep(18,n)
Prob1Knochen_Joint18 <- data.frame(Prob1Knochen_Joint18, joint_Nr)
joint_Nr <- rep(19,n)
Prob1Knochen_Joint19 <- data.frame(Prob1Knochen_Joint19, joint_Nr)
joint_Nr <- rep(20,n)
Prob1Knochen_Joint20 <- data.frame(Prob1Knochen_Joint20, joint_Nr)
joint_Nr <- rep(21,n)
Prob1Knochen_Joint21 <- data.frame(Prob1Knochen_Joint21, joint_Nr)
joint_Nr <- rep(22,n)
Prob1Knochen_Joint22 <- data.frame(Prob1Knochen_Joint22, joint_Nr)
joint_Nr <- rep(23,n)
Prob1Knochen_Joint23 <- data.frame(Prob1Knochen_Joint23, joint_Nr)
joint_Nr <- rep(24,n)
Prob1Knochen_Joint24 <- data.frame(Prob1Knochen_Joint24, joint_Nr)

###### Zusammenzufügen

Prob1Knochen <- rbind(Prob1Knochen_Joint0,Prob1Knochen_Joint1,Prob1Knochen_Joint2,Prob1Knochen_Joint3, Prob1Knochen_Joint4,
                      Prob1Knochen_Joint5,Prob1Knochen_Joint6,Prob1Knochen_Joint7,Prob1Knochen_Joint8, Prob1Knochen_Joint9,
                      Prob1Knochen_Joint10,Prob1Knochen_Joint11,Prob1Knochen_Joint12,Prob1Knochen_Joint13, Prob1Knochen_Joint14,
                      Prob1Knochen_Joint15,Prob1Knochen_Joint16,Prob1Knochen_Joint17,Prob1Knochen_Joint18, Prob1Knochen_Joint19,
                      Prob1Knochen_Joint20,Prob1Knochen_Joint21,Prob1Knochen_Joint22,Prob1Knochen_Joint23, Prob1Knochen_Joint24)

rm(Prob1Knochen_Joint0,Prob1Knochen_Joint1,Prob1Knochen_Joint2,Prob1Knochen_Joint3, Prob1Knochen_Joint4,
   Prob1Knochen_Joint5,Prob1Knochen_Joint6,Prob1Knochen_Joint7,Prob1Knochen_Joint8, Prob1Knochen_Joint9,
   Prob1Knochen_Joint10,Prob1Knochen_Joint11,Prob1Knochen_Joint12,Prob1Knochen_Joint13, Prob1Knochen_Joint14,
   Prob1Knochen_Joint15,Prob1Knochen_Joint16,Prob1Knochen_Joint17,Prob1Knochen_Joint18, Prob1Knochen_Joint19,
   Prob1Knochen_Joint20,Prob1Knochen_Joint21,Prob1Knochen_Joint22,Prob1Knochen_Joint23, Prob1Knochen_Joint24)


# Proband 2 Datensätze zusammenfügen 
n <- nrow(Prob2Knochen_Joint0)

joint_Nr <- rep(0,n)
Prob2Knochen_Joint0 <- data.frame(Prob2Knochen_Joint0, joint_Nr)
joint_Nr <- rep(1,n)
Prob2Knochen_Joint1 <- data.frame(Prob2Knochen_Joint1, joint_Nr)
joint_Nr <- rep(2,n)
Prob2Knochen_Joint2 <- data.frame(Prob2Knochen_Joint2, joint_Nr)
joint_Nr <- rep(3,n)
Prob2Knochen_Joint3 <- data.frame(Prob2Knochen_Joint3, joint_Nr)
joint_Nr <- rep(4,n)
Prob2Knochen_Joint4 <- data.frame(Prob2Knochen_Joint4, joint_Nr)
joint_Nr <- rep(5,n)
Prob2Knochen_Joint5 <- data.frame(Prob2Knochen_Joint5, joint_Nr)
joint_Nr <- rep(6,n)
Prob2Knochen_Joint6 <- data.frame(Prob2Knochen_Joint6, joint_Nr)
joint_Nr <- rep(7,n)
Prob2Knochen_Joint7 <- data.frame(Prob2Knochen_Joint7, joint_Nr)
joint_Nr <- rep(8,n)
Prob2Knochen_Joint8 <- data.frame(Prob2Knochen_Joint8, joint_Nr)
joint_Nr <- rep(9,n)
Prob2Knochen_Joint9 <- data.frame(Prob2Knochen_Joint9, joint_Nr)
joint_Nr <- rep(10,n)
Prob2Knochen_Joint10 <- data.frame(Prob2Knochen_Joint10, joint_Nr)
joint_Nr <- rep(11,n)
Prob2Knochen_Joint11 <- data.frame(Prob2Knochen_Joint11, joint_Nr)
joint_Nr <- rep(12,n)
Prob2Knochen_Joint12 <- data.frame(Prob2Knochen_Joint12, joint_Nr)
joint_Nr <- rep(13,n)
Prob2Knochen_Joint13 <- data.frame(Prob2Knochen_Joint13, joint_Nr)
joint_Nr <- rep(14,n)
Prob2Knochen_Joint14 <- data.frame(Prob2Knochen_Joint14, joint_Nr)
joint_Nr <- rep(15,n)
Prob2Knochen_Joint15 <- data.frame(Prob2Knochen_Joint15, joint_Nr)
joint_Nr <- rep(16,n)
Prob2Knochen_Joint16 <- data.frame(Prob2Knochen_Joint16, joint_Nr)
joint_Nr <- rep(17,n)
Prob2Knochen_Joint17 <- data.frame(Prob2Knochen_Joint17, joint_Nr)
joint_Nr <- rep(18,n)
Prob2Knochen_Joint18 <- data.frame(Prob2Knochen_Joint18, joint_Nr)
joint_Nr <- rep(19,n)
Prob2Knochen_Joint19 <- data.frame(Prob2Knochen_Joint19, joint_Nr)
joint_Nr <- rep(20,n)
Prob2Knochen_Joint20 <- data.frame(Prob2Knochen_Joint20, joint_Nr)
joint_Nr <- rep(21,n)
Prob2Knochen_Joint21 <- data.frame(Prob2Knochen_Joint21, joint_Nr)
joint_Nr <- rep(22,n)
Prob2Knochen_Joint22 <- data.frame(Prob2Knochen_Joint22, joint_Nr)
joint_Nr <- rep(23,n)
Prob2Knochen_Joint23 <- data.frame(Prob2Knochen_Joint23, joint_Nr)
joint_Nr <- rep(24,n)
Prob2Knochen_Joint24 <- data.frame(Prob2Knochen_Joint24, joint_Nr)

###### Zusammenzufügen

Prob2Knochen <- rbind(Prob2Knochen_Joint0,Prob2Knochen_Joint1,Prob2Knochen_Joint2,Prob2Knochen_Joint3, Prob2Knochen_Joint4,
                      Prob2Knochen_Joint5,Prob2Knochen_Joint6,Prob2Knochen_Joint7,Prob2Knochen_Joint8, Prob2Knochen_Joint9,
                      Prob2Knochen_Joint10,Prob2Knochen_Joint11,Prob2Knochen_Joint12,Prob2Knochen_Joint13, Prob2Knochen_Joint14,
                      Prob2Knochen_Joint15,Prob2Knochen_Joint16,Prob2Knochen_Joint17,Prob2Knochen_Joint18, Prob2Knochen_Joint19,
                      Prob2Knochen_Joint20,Prob2Knochen_Joint21,Prob2Knochen_Joint22,Prob2Knochen_Joint23, Prob2Knochen_Joint24)

rm(Prob2Knochen_Joint0,Prob2Knochen_Joint1,Prob2Knochen_Joint2,Prob2Knochen_Joint3, Prob2Knochen_Joint4,
   Prob2Knochen_Joint5,Prob2Knochen_Joint6,Prob2Knochen_Joint7,Prob2Knochen_Joint8, Prob2Knochen_Joint9,
   Prob2Knochen_Joint10,Prob2Knochen_Joint11,Prob2Knochen_Joint12,Prob2Knochen_Joint13, Prob2Knochen_Joint14,
   Prob2Knochen_Joint15,Prob2Knochen_Joint16,Prob2Knochen_Joint17,Prob2Knochen_Joint18, Prob2Knochen_Joint19,
   Prob2Knochen_Joint20,Prob2Knochen_Joint21,Prob2Knochen_Joint22,Prob2Knochen_Joint23, Prob2Knochen_Joint24)



# Proband 1 Datensätze zusammenfügen 
n <- nrow(Prob3Knochen_Joint0)

joint_Nr <- rep(0,n)
Prob3Knochen_Joint0 <- data.frame(Prob3Knochen_Joint0, joint_Nr)
joint_Nr <- rep(1,n)
Prob3Knochen_Joint1 <- data.frame(Prob3Knochen_Joint1, joint_Nr)
joint_Nr <- rep(2,n)
Prob3Knochen_Joint2 <- data.frame(Prob3Knochen_Joint2, joint_Nr)
joint_Nr <- rep(3,n)
Prob3Knochen_Joint3 <- data.frame(Prob3Knochen_Joint3, joint_Nr)
joint_Nr <- rep(4,n)
Prob3Knochen_Joint4 <- data.frame(Prob3Knochen_Joint4, joint_Nr)
joint_Nr <- rep(5,n)
Prob3Knochen_Joint5 <- data.frame(Prob3Knochen_Joint5, joint_Nr)
joint_Nr <- rep(6,n)
Prob3Knochen_Joint6 <- data.frame(Prob3Knochen_Joint6, joint_Nr)
joint_Nr <- rep(7,n)
Prob3Knochen_Joint7 <- data.frame(Prob3Knochen_Joint7, joint_Nr)
joint_Nr <- rep(8,n)
Prob3Knochen_Joint8 <- data.frame(Prob3Knochen_Joint8, joint_Nr)
joint_Nr <- rep(9,n)
Prob3Knochen_Joint9 <- data.frame(Prob3Knochen_Joint9, joint_Nr)
joint_Nr <- rep(10,n)
Prob3Knochen_Joint10 <- data.frame(Prob3Knochen_Joint10, joint_Nr)
joint_Nr <- rep(11,n)
Prob3Knochen_Joint11 <- data.frame(Prob3Knochen_Joint11, joint_Nr)
joint_Nr <- rep(12,n)
Prob3Knochen_Joint12 <- data.frame(Prob3Knochen_Joint12, joint_Nr)
joint_Nr <- rep(13,n)
Prob3Knochen_Joint13 <- data.frame(Prob3Knochen_Joint13, joint_Nr)
joint_Nr <- rep(14,n)
Prob3Knochen_Joint14 <- data.frame(Prob3Knochen_Joint14, joint_Nr)
joint_Nr <- rep(15,n)
Prob3Knochen_Joint15 <- data.frame(Prob3Knochen_Joint15, joint_Nr)
joint_Nr <- rep(16,n)
Prob3Knochen_Joint16 <- data.frame(Prob3Knochen_Joint16, joint_Nr)
joint_Nr <- rep(17,n)
Prob3Knochen_Joint17 <- data.frame(Prob3Knochen_Joint17, joint_Nr)
joint_Nr <- rep(18,n)
Prob3Knochen_Joint18 <- data.frame(Prob3Knochen_Joint18, joint_Nr)
joint_Nr <- rep(19,n)
Prob3Knochen_Joint19 <- data.frame(Prob3Knochen_Joint19, joint_Nr)
joint_Nr <- rep(20,n)
Prob3Knochen_Joint20 <- data.frame(Prob3Knochen_Joint20, joint_Nr)
joint_Nr <- rep(21,n)
Prob3Knochen_Joint21 <- data.frame(Prob3Knochen_Joint21, joint_Nr)
joint_Nr <- rep(22,n)
Prob3Knochen_Joint22 <- data.frame(Prob3Knochen_Joint22, joint_Nr)
joint_Nr <- rep(23,n)
Prob3Knochen_Joint23 <- data.frame(Prob3Knochen_Joint23, joint_Nr)
joint_Nr <- rep(24,n)
Prob3Knochen_Joint24 <- data.frame(Prob3Knochen_Joint24, joint_Nr)

###### Zusammenzufügen

Prob3Knochen <- rbind(Prob3Knochen_Joint0,Prob3Knochen_Joint1,Prob3Knochen_Joint2,Prob3Knochen_Joint3, Prob3Knochen_Joint4,
                      Prob3Knochen_Joint5,Prob3Knochen_Joint6,Prob3Knochen_Joint7,Prob3Knochen_Joint8, Prob3Knochen_Joint9,
                      Prob3Knochen_Joint10,Prob3Knochen_Joint11,Prob3Knochen_Joint12,Prob3Knochen_Joint13, Prob3Knochen_Joint14,
                      Prob3Knochen_Joint15,Prob3Knochen_Joint16,Prob3Knochen_Joint17,Prob3Knochen_Joint18, Prob3Knochen_Joint19,
                      Prob3Knochen_Joint20,Prob3Knochen_Joint21,Prob3Knochen_Joint22,Prob3Knochen_Joint23, Prob3Knochen_Joint24)

rm(Prob3Knochen_Joint0,Prob3Knochen_Joint1,Prob3Knochen_Joint2,Prob3Knochen_Joint3, Prob3Knochen_Joint4,
   Prob3Knochen_Joint5,Prob3Knochen_Joint6,Prob3Knochen_Joint7,Prob3Knochen_Joint8, Prob3Knochen_Joint9,
   Prob3Knochen_Joint10,Prob3Knochen_Joint11,Prob3Knochen_Joint12,Prob3Knochen_Joint13, Prob3Knochen_Joint14,
   Prob3Knochen_Joint15,Prob3Knochen_Joint16,Prob3Knochen_Joint17,Prob3Knochen_Joint18, Prob3Knochen_Joint19,
   Prob3Knochen_Joint20,Prob3Knochen_Joint21,Prob3Knochen_Joint22,Prob3Knochen_Joint23, Prob3Knochen_Joint24)


# Proband 4 Datensätze zusammenfügen 
n <- nrow(Prob4Knochen_Joint0)

joint_Nr <- rep(0,n)
Prob4Knochen_Joint0 <- data.frame(Prob4Knochen_Joint0, joint_Nr)
joint_Nr <- rep(1,n)
Prob4Knochen_Joint1 <- data.frame(Prob4Knochen_Joint1, joint_Nr)
joint_Nr <- rep(2,n)
Prob4Knochen_Joint2 <- data.frame(Prob4Knochen_Joint2, joint_Nr)
joint_Nr <- rep(3,n)
Prob4Knochen_Joint3 <- data.frame(Prob4Knochen_Joint3, joint_Nr)
joint_Nr <- rep(4,n)
Prob4Knochen_Joint4 <- data.frame(Prob4Knochen_Joint4, joint_Nr)
joint_Nr <- rep(5,n)
Prob4Knochen_Joint5 <- data.frame(Prob4Knochen_Joint5, joint_Nr)
joint_Nr <- rep(6,n)
Prob4Knochen_Joint6 <- data.frame(Prob4Knochen_Joint6, joint_Nr)
joint_Nr <- rep(7,n)
Prob4Knochen_Joint7 <- data.frame(Prob4Knochen_Joint7, joint_Nr)
joint_Nr <- rep(8,n)
Prob4Knochen_Joint8 <- data.frame(Prob4Knochen_Joint8, joint_Nr)
joint_Nr <- rep(9,n)
Prob4Knochen_Joint9 <- data.frame(Prob4Knochen_Joint9, joint_Nr)
joint_Nr <- rep(10,n)
Prob4Knochen_Joint10 <- data.frame(Prob4Knochen_Joint10, joint_Nr)
joint_Nr <- rep(11,n)
Prob4Knochen_Joint11 <- data.frame(Prob4Knochen_Joint11, joint_Nr)
joint_Nr <- rep(12,n)
Prob4Knochen_Joint12 <- data.frame(Prob4Knochen_Joint12, joint_Nr)
joint_Nr <- rep(13,n)
Prob4Knochen_Joint13 <- data.frame(Prob4Knochen_Joint13, joint_Nr)
joint_Nr <- rep(14,n)
Prob4Knochen_Joint14 <- data.frame(Prob4Knochen_Joint14, joint_Nr)
joint_Nr <- rep(15,n)
Prob4Knochen_Joint15 <- data.frame(Prob4Knochen_Joint15, joint_Nr)
joint_Nr <- rep(16,n)
Prob4Knochen_Joint16 <- data.frame(Prob4Knochen_Joint16, joint_Nr)
joint_Nr <- rep(17,n)
Prob4Knochen_Joint17 <- data.frame(Prob4Knochen_Joint17, joint_Nr)
joint_Nr <- rep(18,n)
Prob4Knochen_Joint18 <- data.frame(Prob4Knochen_Joint18, joint_Nr)
joint_Nr <- rep(19,n)
Prob4Knochen_Joint19 <- data.frame(Prob4Knochen_Joint19, joint_Nr)
joint_Nr <- rep(20,n)
Prob4Knochen_Joint20 <- data.frame(Prob4Knochen_Joint20, joint_Nr)
joint_Nr <- rep(21,n)
Prob4Knochen_Joint21 <- data.frame(Prob4Knochen_Joint21, joint_Nr)
joint_Nr <- rep(22,n)
Prob4Knochen_Joint22 <- data.frame(Prob4Knochen_Joint22, joint_Nr)
joint_Nr <- rep(23,n)
Prob4Knochen_Joint23 <- data.frame(Prob4Knochen_Joint23, joint_Nr)
joint_Nr <- rep(24,n)
Prob4Knochen_Joint24 <- data.frame(Prob4Knochen_Joint24, joint_Nr)

###### Zusammenzufügen

Prob4Knochen <- rbind(Prob4Knochen_Joint0,Prob4Knochen_Joint1,Prob4Knochen_Joint2,Prob4Knochen_Joint3, Prob4Knochen_Joint4,
                      Prob4Knochen_Joint5,Prob4Knochen_Joint6,Prob4Knochen_Joint7,Prob4Knochen_Joint8, Prob4Knochen_Joint9,
                      Prob4Knochen_Joint10,Prob4Knochen_Joint11,Prob4Knochen_Joint12,Prob4Knochen_Joint13, Prob4Knochen_Joint14,
                      Prob4Knochen_Joint15,Prob4Knochen_Joint16,Prob4Knochen_Joint17,Prob4Knochen_Joint18, Prob4Knochen_Joint19,
                      Prob4Knochen_Joint20,Prob4Knochen_Joint21,Prob4Knochen_Joint22,Prob4Knochen_Joint23, Prob4Knochen_Joint24)

rm(Prob4Knochen_Joint0,Prob4Knochen_Joint1,Prob4Knochen_Joint2,Prob4Knochen_Joint3, Prob4Knochen_Joint4,
   Prob4Knochen_Joint5,Prob4Knochen_Joint6,Prob4Knochen_Joint7,Prob4Knochen_Joint8, Prob4Knochen_Joint9,
   Prob4Knochen_Joint10,Prob4Knochen_Joint11,Prob4Knochen_Joint12,Prob4Knochen_Joint13, Prob4Knochen_Joint14,
   Prob4Knochen_Joint15,Prob4Knochen_Joint16,Prob4Knochen_Joint17,Prob4Knochen_Joint18, Prob4Knochen_Joint19,
   Prob4Knochen_Joint20,Prob4Knochen_Joint21,Prob4Knochen_Joint22,Prob4Knochen_Joint23, Prob4Knochen_Joint24)


# Proband 5 Datensätze zusammenfügen 
n <- nrow(Prob5Knochen_Joint0)

joint_Nr <- rep(0,n)
Prob5Knochen_Joint0 <- data.frame(Prob5Knochen_Joint0, joint_Nr)
joint_Nr <- rep(1,n)
Prob5Knochen_Joint1 <- data.frame(Prob5Knochen_Joint1, joint_Nr)
joint_Nr <- rep(2,n)
Prob5Knochen_Joint2 <- data.frame(Prob5Knochen_Joint2, joint_Nr)
joint_Nr <- rep(3,n)
Prob5Knochen_Joint3 <- data.frame(Prob5Knochen_Joint3, joint_Nr)
joint_Nr <- rep(4,n)
Prob5Knochen_Joint4 <- data.frame(Prob5Knochen_Joint4, joint_Nr)
joint_Nr <- rep(5,n)
Prob5Knochen_Joint5 <- data.frame(Prob5Knochen_Joint5, joint_Nr)
joint_Nr <- rep(6,n)
Prob5Knochen_Joint6 <- data.frame(Prob5Knochen_Joint6, joint_Nr)
joint_Nr <- rep(7,n)
Prob5Knochen_Joint7 <- data.frame(Prob5Knochen_Joint7, joint_Nr)
joint_Nr <- rep(8,n)
Prob5Knochen_Joint8 <- data.frame(Prob5Knochen_Joint8, joint_Nr)
joint_Nr <- rep(9,n)
Prob5Knochen_Joint9 <- data.frame(Prob5Knochen_Joint9, joint_Nr)
joint_Nr <- rep(10,n)
Prob5Knochen_Joint10 <- data.frame(Prob5Knochen_Joint10, joint_Nr)
joint_Nr <- rep(11,n)
Prob5Knochen_Joint11 <- data.frame(Prob5Knochen_Joint11, joint_Nr)
joint_Nr <- rep(12,n)
Prob5Knochen_Joint12 <- data.frame(Prob5Knochen_Joint12, joint_Nr)
joint_Nr <- rep(13,n)
Prob5Knochen_Joint13 <- data.frame(Prob5Knochen_Joint13, joint_Nr)
joint_Nr <- rep(14,n)
Prob5Knochen_Joint14 <- data.frame(Prob5Knochen_Joint14, joint_Nr)
joint_Nr <- rep(15,n)
Prob5Knochen_Joint15 <- data.frame(Prob5Knochen_Joint15, joint_Nr)
joint_Nr <- rep(16,n)
Prob5Knochen_Joint16 <- data.frame(Prob5Knochen_Joint16, joint_Nr)
joint_Nr <- rep(17,n)
Prob5Knochen_Joint17 <- data.frame(Prob5Knochen_Joint17, joint_Nr)
joint_Nr <- rep(18,n)
Prob5Knochen_Joint18 <- data.frame(Prob5Knochen_Joint18, joint_Nr)
joint_Nr <- rep(19,n)
Prob5Knochen_Joint19 <- data.frame(Prob5Knochen_Joint19, joint_Nr)
joint_Nr <- rep(20,n)
Prob5Knochen_Joint20 <- data.frame(Prob5Knochen_Joint20, joint_Nr)
joint_Nr <- rep(21,n)
Prob5Knochen_Joint21 <- data.frame(Prob5Knochen_Joint21, joint_Nr)
joint_Nr <- rep(22,n)
Prob5Knochen_Joint22 <- data.frame(Prob5Knochen_Joint22, joint_Nr)
joint_Nr <- rep(23,n)
Prob5Knochen_Joint23 <- data.frame(Prob5Knochen_Joint23, joint_Nr)
joint_Nr <- rep(24,n)
Prob5Knochen_Joint24 <- data.frame(Prob5Knochen_Joint24, joint_Nr)

###### Zusammenzufügen

Prob5Knochen <- rbind(Prob5Knochen_Joint0,Prob5Knochen_Joint1,Prob5Knochen_Joint2,Prob5Knochen_Joint3, Prob5Knochen_Joint4,
                      Prob5Knochen_Joint5,Prob5Knochen_Joint6,Prob5Knochen_Joint7,Prob5Knochen_Joint8, Prob5Knochen_Joint9,
                      Prob5Knochen_Joint10,Prob5Knochen_Joint11,Prob5Knochen_Joint12,Prob5Knochen_Joint13, Prob5Knochen_Joint14,
                      Prob5Knochen_Joint15,Prob5Knochen_Joint16,Prob5Knochen_Joint17,Prob5Knochen_Joint18, Prob5Knochen_Joint19,
                      Prob5Knochen_Joint20,Prob5Knochen_Joint21,Prob5Knochen_Joint22,Prob5Knochen_Joint23, Prob5Knochen_Joint24)

rm(Prob5Knochen_Joint0,Prob5Knochen_Joint1,Prob5Knochen_Joint2,Prob5Knochen_Joint3, Prob5Knochen_Joint4,
   Prob5Knochen_Joint5,Prob5Knochen_Joint6,Prob5Knochen_Joint7,Prob5Knochen_Joint8, Prob5Knochen_Joint9,
   Prob5Knochen_Joint10,Prob5Knochen_Joint11,Prob5Knochen_Joint12,Prob5Knochen_Joint13, Prob5Knochen_Joint14,
   Prob5Knochen_Joint15,Prob5Knochen_Joint16,Prob5Knochen_Joint17,Prob5Knochen_Joint18, Prob5Knochen_Joint19,
   Prob5Knochen_Joint20,Prob5Knochen_Joint21,Prob5Knochen_Joint22,Prob5Knochen_Joint23, Prob5Knochen_Joint24)



###############Enddatensatz
rm(data, bone_data)

Knochenvariable <- rbind(Prob1Knochen, Prob2Knochen, Prob3Knochen, Prob4Knochen, Prob5Knochen)


rm(Prob1Knochen, Prob2Knochen, Prob3Knochen, Prob4Knochen, Prob5Knochen, jointmat,Jointmatrix_sortiert)
rm(a, abst, bone_error, i, j, joint_Nr, k, l,m,mat_diff,n,smallest,sorted)



