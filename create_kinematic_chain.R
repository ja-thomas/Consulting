library(MoCap)


data("joint_numbers")
joint_numbers

kinematic_model <- matrix(0, nrow = 25, ncol = 25)
rownames(kinematic_model) <- joint_numbers$Joint
colnames(kinematic_model) <- joint_numbers$Joint

#AnkleLeft <-> FootLeft, KneeLeft
kinematic_model[which(rownames(kinematic_model) == "AnkleLeft"), which(colnames(kinematic_model) %in% c("FootLeft", "KneeLeft"))] <- 1

#AnkleRight <-> FootRight, KneeRight
kinematic_model[which(rownames(kinematic_model) == "AnkleRight"), which(colnames(kinematic_model) %in% c("FootRight", "KneeRight"))] <- 1

#ElbowLeft <-> ShoulderLeft, WristLeft
kinematic_model[which(rownames(kinematic_model) == "ElbowLeft"), which(colnames(kinematic_model) %in% c("ShoulderLeft", "WristLeft"))] <- 1

#ElbowRight <-> ShoulderRight, WristRight
kinematic_model[which(rownames(kinematic_model) == "ElbowRight"), which(colnames(kinematic_model) %in% c("ShoulderRight", "WristRight"))] <- 1

#FootLeft <-> AnkleLeft
kinematic_model[which(rownames(kinematic_model) == "FootLeft"), which(colnames(kinematic_model) %in% c("AnkleLeft"))] <- 1

#FootRight <-> AnkleRight
kinematic_model[which(rownames(kinematic_model) == "FootRight"), which(colnames(kinematic_model) %in% c("AnkleRight"))] <- 1

#HandLeft <-> WristLeft, HandTipLeft
kinematic_model[which(rownames(kinematic_model) == "HandLeft"), which(colnames(kinematic_model) %in% c("WristLeft", "HandTipLeft"))] <- 1

#HandRight <-> WristRight, HandTipRight
kinematic_model[which(rownames(kinematic_model) == "HandRight"), which(colnames(kinematic_model) %in% c("WristRight", "HandTipRight"))] <- 1

#HandTipLeft <-> HandLeft
kinematic_model[which(rownames(kinematic_model) == "HandTipLeft"), which(colnames(kinematic_model) %in% c("HandLeft"))] <- 1

#HandTipRight <-> HandRight
kinematic_model[which(rownames(kinematic_model) == "HandTipRight"), which(colnames(kinematic_model) %in% c("HandRight"))] <- 1

#Head <-> Neck
kinematic_model[which(rownames(kinematic_model) == "Head"), which(colnames(kinematic_model) %in% c("Neck"))] <- 1

#HipLeft <-> SpineBase, KneeLeft
kinematic_model[which(rownames(kinematic_model) == "HipLeft"), which(colnames(kinematic_model) %in% c("SpineBase", "KneeLeft"))] <- 1

#HipRight <-> SpineBase, KneeRight
kinematic_model[which(rownames(kinematic_model) == "HipRight"), which(colnames(kinematic_model) %in% c("SpineBase", "KneeRight"))] <- 1

#KneeLeft <-> HipLeft, AnkleLeft
kinematic_model[which(rownames(kinematic_model) == "KneeLeft"), which(colnames(kinematic_model) %in% c("HipLeft", "AnkleLeft"))] <- 1

#KneeRight <-> HipRight, AnkleRight
kinematic_model[which(rownames(kinematic_model) == "KneeRight"), which(colnames(kinematic_model) %in% c("HipRight", "AnkleRight"))] <- 1

#Neck <-> Head, SpineShoulder
kinematic_model[which(rownames(kinematic_model) == "Neck"), which(colnames(kinematic_model) %in% c("Head", "SpineShoulder"))] <- 1

#ShoulderLeft <-> ElbowLeft, SpineShoulder
kinematic_model[which(rownames(kinematic_model) == "ShoulderLeft"), which(colnames(kinematic_model) %in% c("ElbowLeft", "SpineShoulder"))] <- 1

#ShoulderRight <-> ElbowRight, SpineShoulder
kinematic_model[which(rownames(kinematic_model) == "ShoulderRight"), which(colnames(kinematic_model) %in% c("ElbowRight", "SpineShoulder"))] <- 1

#SpineBase <-> HipLeft, HipRight, SpineMid
kinematic_model[which(rownames(kinematic_model) == "SpineBase"), which(colnames(kinematic_model) %in% c("HipLeft", "HipRight", "SpineMid"))] <- 1

#SpineMid <-> SpineShoulder, SpineBase
kinematic_model[which(rownames(kinematic_model) == "SpineMid"), which(colnames(kinematic_model) %in% c("SpineShoulder", "SpineBase"))] <- 1

#SpineShoulder <-> Neck, ShoulderLeft, ShoulderRight, SpineMid
kinematic_model[which(rownames(kinematic_model) == "SpineShoulder"), which(colnames(kinematic_model) %in% c("Neck", "ShoulderLeft", "ShoulderRight", "SpineMid"))] <- 1

#ThumbLeft <-> WristLeft
kinematic_model[which(rownames(kinematic_model) == "ThumbLeft"), which(colnames(kinematic_model) %in% c("WristLeft"))] <- 1

#ThumbRight <-> WristRight
kinematic_model[which(rownames(kinematic_model) == "ThumbRight"), which(colnames(kinematic_model) %in% c("WristRight"))] <- 1

#WristLeft <-> ThumbLeft, HandLeft, ElbowLeft
kinematic_model[which(rownames(kinematic_model) == "WristLeft"), which(colnames(kinematic_model) %in% c("ThumbLeft", "HandLeft", "ElbowLeft"))] <- 1

#WristRight <-> ThumbRight, HandRight, ElbowRight
kinematic_model[which(rownames(kinematic_model) == "WristRight"), which(colnames(kinematic_model) %in% c("ThumbRight", "HandRight", "ElbowRight"))] <- 1
