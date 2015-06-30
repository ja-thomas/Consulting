########################## Datensätze ummodelieren um Korrelation zu berechnen

install.packages("reshape")
library(reshape)

###################################

# benötigte Variablen auswählen
attach(data)
corr_data <- data.frame(timestamp, course_Id, joint_Nr, person, sensorId, deviation)
detach(data)


################ Aufsplitten nach Personen, da Datensatz sonst zu groß

corr_1 <- subset(corr_data, corr_data$person == "Proband 1")
corr_2 <- subset(corr_data, corr_data$person == "Proband 2")
corr_3 <- subset(corr_data, corr_data$person == "Proband 3")
corr_4 <- subset(corr_data, corr_data$person == "Proband 4")
corr_5 <- subset(corr_data, corr_data$person == "Proband 5")



#Korrelation zwischen den einzelnen Kameras


# Datensatz erstellen, mit eigener Spalte für jede Kamera, die Abweichung vom Optitrack angibt

# Datensatz für Proband 1
corr_cam1 <- corr_1

corr_cam1 <- melt(corr_cam1, id = c("timestamp", "course_Id" "sensorId", "joint_Nr", "person"), measure = c("deviation"))
corr_cam1 <- cast(corr_cam1, timestamp + course_Id + joint_Nr + person ~ sensorId)

# Datensatz für Proband 2
corr_cam2 <- corr_2

corr_cam2 <- melt(corr_cam2, id = c("timestamp", "course_Id" "sensorId", "joint_Nr", "person"), measure = c("deviation"))
corr_cam2 <- cast(corr_cam2, timestamp + course_Id + joint_Nr + person ~ sensorId)

# Datensatz für Proband 3
corr_cam3 <- corr_3

corr_cam3 <- melt(corr_cam3, id = c("timestamp", "course_Id" "sensorId", "joint_Nr", "person"), measure = c("deviation"))
corr_cam3 <- cast(corr_cam3, timestamp + course_Id + joint_Nr + person ~ sensorId)

# Datensatz für Proband 4
corr_cam4 <- corr_4

corr_cam4 <- melt(corr_cam4, id = c("timestamp", "course_Id" "sensorId", "joint_Nr", "person"), measure = c("deviation"))
corr_cam4 <- cast(corr_cam4, timestamp + course_Id + joint_Nr + person ~ sensorId)

# Datensatz für Proband 5
corr_cam5 <- corr_5

corr_cam5 <- melt(corr_cam5, id = c("timestamp", "course_Id" "sensorId", "joint_Nr", "person"), measure = c("deviation"))
corr_cam5 <- cast(corr_cam5, timestamp + course_Id + joint_Nr + person ~ sensorId)

#### Teildatensätze zusammenfügen

corr_cam <- rbind(corr_cam1, corr_cam2, corr_cam3, corr_cam4, corr_cam5)
rm(corr_cam1, corr_cam2, corr_cam3, corr_cam4, corr_cam5)




####### Korrelation berechnen


matrix_corr_cam <- cor(corr_cam[,c(5,6,7,8,9,10)], use = "pairwise.complete.obs")
colnames(matrix_corr_cam) <- c("007319145247", "011921745247", "500005441742",
                               "500718743142", "500860243142", "501958741942")
rownames(matrix_corr_cam) <- c("007319145247", "011921745247", "500005441742",
                                  "500718743142", "500860243142", "501958741942")


##### Visualisierung der Korrelationen

#install.packages("corrplot")
library(corrplot)


corrplot(matrix_corr_cam ,method="circle", type = "lower")


########################## Korrelation der Abweichungen für die 25 Joints berechnen




# Datensatz erstellen, mit eigener Spalte für jeden Joint, die Abweichung vom Optitrack angibt

# Datensatz für Proband 1
corr_joint1 <- corr_1

corr_joint1 <- melt(corr_joint1, id = c("timestamp", "course_Id" "sensorId", "joint_Nr", "person"), measure = c("deviation"))
corr_joint1 <- cast(corr_joint1, timestamp + course_Id + sensorId + person ~ joint_Nr)
rm(corr_1)
# Datensatz für Proband 2
corr_joint2 <- corr_2

corr_joint2 <- melt(corr_joint2, id = c("timestamp", "course_Id" "sensorId", "joint_Nr", "person"), measure = c("deviation"))
corr_joint2 <- cast(corr_joint2, timestamp + course_Id + sensorId + person ~ joint_Nr)
rm(corr_2)
# Datensatz für Proband 3
corr_joint3 <- corr_3

corr_joint3 <- melt(corr_joint3, id = c("timestamp", "course_Id" "sensorId", "joint_Nr", "person"), measure = c("deviation"))
corr_joint3 <- cast(corr_joint3, timestamp + course_Id + sensorId + person ~ joint_Nr)
rm(corr_3)
# Datensatz für Proband 4
corr_joint4 <- corr_4

corr_joint4 <- melt(corr_joint4, id = c("timestamp", "course_Id" "sensorId", "joint_Nr", "person"), measure = c("deviation"))
corr_joint4<- cast(corr_joint4, timestamp + course_Id + sensorId + person ~ joint_Nr)
rm(corr_4)
# Datensatz für Proband 5
corr_joint5 <- corr_5

corr_joint5 <- melt(corr_joint5, id = c("timestamp", "course_Id" "sensorId", "joint_Nr", "person"), measure = c("deviation"))
corr_joint5 <- cast(corr_joint5, timestamp + course_Id + sensorId + person ~ joint_Nr)
rm(corr_5)
#### Teildatensätze zusammenfügen

corr_joint <- rbind(corr_joint1, corr_joint2, corr_joint3, corr_joint4, corr_joint5)
rm(corr_joint1, corr_joint2, corr_joint3, corr_joint4, corr_joint5)

####### Korrelation berechnen


matrix_corr_joint <- cor(corr_joint[,-c(1,2,3,4)] , use = "pairwise.complete.obs")

colnames(matrix_corr_joint) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                  "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                  "21", "22", "23", "24")
rownames(matrix_corr_joint) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                 "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                 "21", "22", "23", "24")

##### Visualisierung der Korrelationen

#install.packages("corrplot")
library(corrplot)

jpeg('Corr_Joints.jpg')
corrplot(matrix_corr_joint ,method="circle", type = "lower")
dev.off()


################## nur Korrelation verbundener Matrizen

Jointmatrix_sortiert <- read.csv("~/Consulting/Jointmatrix_sortiert.csv", sep=";")
joint_mat<- Jointmatrix_sortiert[,-1]
rm(Jointmatrix_sortiert)

con_corr_joint <- NULL
for (i in 1:25){
  for(j in 1:25){
     a <- joint_mat[i,j]*matrix_corr_joint[i,j]
     con_corr_joint <- c(con_corr_joint,a) 
  }
}

con_corr_joint <- matrix(con_corr_joint, nrow = 25, ncol = 25)
colnames(con_corr_joint) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                 "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                 "21", "22", "23", "24")
rownames(con_corr_joint) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                 "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                 "21", "22", "23", "24")


corrplot(con_corr_joint ,method="circle", type = "lower")

######## berechnen der mittleren Korrelation der verbundenen Joints

con_corr_joint_sort <- sort(con_corr_joint)
con_corr_joint_sort <- con_corr_joint_sort[578:625]
mean(con_corr_joint_sort) # alte Daten mittlere Korrelation von 0.92 !
