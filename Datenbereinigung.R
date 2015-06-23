############## Datenbereinigung

work_data <- data # Arbeitsdatensatz definieren

############ Löschen von Proband 3, Bewegungsablauf Vor und zurück

prob3_vor <- work_data2[(work_data$person == "Proband 3") & (work_data$course_Id == "Vor zurueck"),] # Herauspicken von Proband 3, Vor und Zurücklaufen
plot(prob3_vor$deviation, type = "l") 
plot(prob3_vor$acceleration, type = "l") # Grafiken zeigen viele unplausible Werte
rm(prob3_vor)

# Entfernen aller Werte von Proband 3 mit Bewegungsablauf Vor und zurück
work_data <- work_data[!((work_data$person == "Proband 3") & -(work_data$course_Id == "Vor zurueck")),]


################## Löschen aller Fälle, mit z-Wert kleiner 0

data <- subset(work_data, work_data$position_z >= 0)





