rm(list = ls()) 
library(tidyverse)
shai <- read.csv("../0_shaixuan/shai.csv")
#Cut out the unnecessary
shai <- shai[,-1]


#SBP
SBP <- read.csv("ICU_admission/SBP.csv")
SBP <- SBP[,c(4,6,10)]
colnames(SBP) <- c("icustay_id","time_SBP","value_SBP")
SBP$icustay_id <- as.integer(SBP$icustay_id)
df <- inner_join(shai,SBP,by="icustay_id")
df$time_SBP <- as.POSIXct(df$time_SBP)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_SBP>=(df$in_time-21600) & df$time_SBP<=(df$in_time+86400),]
df <- df[complete.cases(df$value_SBP),]
df <- df[order(df$hadm_id,df$time_SBP),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
SBP <- as.data.frame(cbind(df$hadm_id,df$value_SBP))
colnames(SBP) <- c("hadm_id","value_SBP")
SBP$hadm_id <- as.integer(SBP$hadm_id)
shai <- left_join(shai,SBP,by="hadm_id")


#DBP
DBP <- read.csv("ICU_admission/DBP.csv")
DBP <- DBP[,c(4,6,10)]
colnames(DBP) <- c("icustay_id","time_DBP","value_DBP")
DBP$icustay_id <- as.integer(DBP$icustay_id)
df <- inner_join(shai,DBP,by="icustay_id")
df$time_DBP <- as.POSIXct(df$time_DBP)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_DBP>=(df$in_time-21600) & df$time_DBP<=(df$in_time+86400),]
df <- df[complete.cases(df$value_DBP),]
df <- df[order(df$hadm_id,df$time_DBP),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
DBP <- as.data.frame(cbind(df$hadm_id,df$value_DBP))
colnames(DBP) <- c("hadm_id","value_DBP")
DBP$hadm_id <- as.integer(DBP$hadm_id)
shai <- left_join(shai,DBP,by="hadm_id")


#MBP
MBP <- read.csv("ICU_admission/MBP.csv")
MBP <- MBP[,c(4,6,10)]
colnames(MBP) <- c("icustay_id","time_MBP","value_MBP")
MBP$icustay_id <- as.integer(MBP$icustay_id)
df <- inner_join(shai,MBP,by="icustay_id")
df$time_MBP <- as.POSIXct(df$time_MBP)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_MBP>=(df$in_time-21600) & df$time_MBP<=(df$in_time+86400),]
df <- df[complete.cases(df$value_MBP),]
df <- df[order(df$hadm_id,df$time_MBP),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
MBP <- as.data.frame(cbind(df$hadm_id,df$value_MBP))
colnames(MBP) <- c("hadm_id","value_MBP")
MBP$hadm_id <- as.integer(MBP$hadm_id)
shai <- left_join(shai,MBP,by="hadm_id")


#HR
HR <- read.csv("ICU_admission/HR.csv")
HR <- HR[,c(4,6,10)]
colnames(HR) <- c("icustay_id","time_HR","value_HR")
HR$icustay_id <- as.integer(HR$icustay_id)
df <- inner_join(shai,HR,by="icustay_id")
df$time_HR <- as.POSIXct(df$time_HR)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_HR>=(df$in_time-21600) & df$time_HR<=(df$in_time+86400),]
df <- df[complete.cases(df$value_HR),]
df <- df[order(df$hadm_id,df$time_HR),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
HR <- as.data.frame(cbind(df$hadm_id,df$value_HR))
colnames(HR) <- c("hadm_id","value_HR")
HR$hadm_id <- as.integer(HR$hadm_id)
shai <- left_join(shai,HR,by="hadm_id")


#Respiratory rate
Rr <- read.csv("ICU_admission/Rr.csv")
Rr <- Rr[,c(4,6,10)]
colnames(Rr) <- c("icustay_id","time_Rr","value_Rr")
Rr$icustay_id <- as.integer(Rr$icustay_id)
df <- inner_join(shai,Rr,by="icustay_id")
df$time_Rr <- as.POSIXct(df$time_Rr)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_Rr>=(df$in_time-21600) & df$time_Rr<=(df$in_time+86400),]
df <- df[complete.cases(df$value_Rr),]
df <- df[order(df$hadm_id,df$time_Rr),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
Rr <- as.data.frame(cbind(df$hadm_id,df$value_Rr))
colnames(Rr) <- c("hadm_id","value_Rr")
Rr$hadm_id <- as.integer(Rr$hadm_id)
shai <- left_join(shai,Rr,by="hadm_id")


#Temperature
Temperature <- read.csv("ICU_admission/Temperature.csv")
Temperature <- Temperature[,c(4,6,16)]
colnames(Temperature) <- c("icustay_id","time_Temperature","value_Temperature")
Temperature$icustay_id <- as.integer(Temperature$icustay_id)
df <- inner_join(shai,Temperature,by="icustay_id")
df$time_Temperature <- as.POSIXct(df$time_Temperature)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_Temperature>=(df$in_time-21600) & df$time_Temperature<=(df$in_time+86400),]
df <- df[complete.cases(df$value_Temperature),]
df <- df[order(df$hadm_id,df$time_Temperature),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
Temperature <- as.data.frame(cbind(df$hadm_id,df$value_Temperature))
colnames(Temperature) <- c("hadm_id","value_Temperature")
Temperature$hadm_id <- as.integer(Temperature$hadm_id)
shai <- left_join(shai,Temperature,by="hadm_id")


#SPO2
SPO2 <- read.csv("ICU_admission/SPO2.csv")
SPO2 <- SPO2[,c(4,6,10)]
colnames(SPO2) <- c("icustay_id","time_SPO2","value_SPO2")
SPO2$icustay_id <- as.integer(SPO2$icustay_id)
df <- inner_join(shai,SPO2,by="icustay_id")
df$time_SPO2 <- as.POSIXct(df$time_SPO2)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_SPO2>=(df$in_time-21600) & df$time_SPO2<=(df$in_time+86400),]
df <- df[complete.cases(df$value_SPO2),]
df <- df[order(df$hadm_id,df$time_SPO2),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
SPO2 <- as.data.frame(cbind(df$hadm_id,df$value_SPO2))
colnames(SPO2) <- c("hadm_id","value_SPO2")
SPO2$hadm_id <- as.integer(SPO2$hadm_id)
shai <- left_join(shai,SPO2,by="hadm_id")

a <- read.csv("./Comorbidities/d_icd_diagnoses.csv")
diagnoses <- read.csv("./Comorbidities/diagnoses_icd.csv")
#According to ICD-9code
#CHD
b <- a[c(grep("^410",a$icd9_code),
         grep("^411",a$icd9_code),
         grep("^412",a$icd9_code),
         grep("^413",a$icd9_code),
         grep("^414",a$icd9_code)),]
code <- b$icd9_code
CHD <- diagnoses[which(diagnoses$icd9_code %in% code),]
CHD <- CHD[,3:4]
colnames(CHD) <- c("hadm_id","CHD")
CHD <- distinct(CHD, hadm_id, .keep_all = TRUE)
shai <- left_join(shai,CHD,by="hadm_id")
shai$CHD[shai$CHD>0] <- 1
shai$CHD[is.na(shai$CHD)] <- 2

#CHF
b <- a[c(grep("^4280",a$icd9_code)),]
code <- b$icd9_code
CHF <- diagnoses[which(diagnoses$icd9_code %in% code),]
CHF <- CHF[,3:4]
colnames(CHF) <- c("hadm_id","CHF")
CHF <- distinct(CHF, hadm_id, .keep_all = TRUE)
shai <- left_join(shai,CHF,by="hadm_id")
shai$CHF[shai$CHF>0] <- 1
shai$CHF[is.na(shai$CHF)] <- 2

#AF
b <- a[c(grep("^42731",a$icd9_code)),]
code <- b$icd9_code
AF <- diagnoses[which(diagnoses$icd9_code %in% code),]
AF <- AF[,3:4]
colnames(AF) <- c("hadm_id","AF")
AF <- distinct(AF, hadm_id, .keep_all = TRUE)
shai <- left_join(shai,AF,by="hadm_id")
shai$AF[shai$AF>0] <- 1
shai$AF[is.na(shai$AF)] <- 2

#Stroke
b <- a[c(grep("^430",a$icd9_code),
         grep("^431",a$icd9_code),
         grep("^432",a$icd9_code),
         grep("^433",a$icd9_code),
         grep("^434",a$icd9_code),
         grep("^435",a$icd9_code),
         grep("^436",a$icd9_code),
         grep("^437",a$icd9_code),
         grep("^438",a$icd9_code)),]
code <- b$icd9_code
Stroke <- diagnoses[which(diagnoses$icd9_code %in% code),]
Stroke <- Stroke[,3:4]
colnames(Stroke) <- c("hadm_id","Stroke")
Stroke <- distinct(Stroke, hadm_id, .keep_all = TRUE)
shai <- left_join(shai,Stroke,by="hadm_id")
shai$Stroke[shai$Stroke>0] <- 1
shai$Stroke[is.na(shai$Stroke)] <- 2

#RD
b <- a[c(grep("^580",a$icd9_code),
         grep("^481",a$icd9_code),
         grep("^482",a$icd9_code),
         grep("^483",a$icd9_code),
         grep("^484",a$icd9_code),
         grep("^485",a$icd9_code),
         grep("^486",a$icd9_code),
         grep("^487",a$icd9_code),
         grep("^488",a$icd9_code),
         grep("^489",a$icd9_code)),]
code <- b$icd9_code
RD <- diagnoses[which(diagnoses$icd9_code %in% code),]
RD <- RD[,3:4]
colnames(RD) <- c("hadm_id","RD")
RD <- distinct(RD, hadm_id, .keep_all = TRUE)
shai <- left_join(shai,RD,by="hadm_id")
shai$RD[shai$RD>0] <- 1
shai$RD[is.na(shai$RD)] <- 2
#LD
b <- a[c(grep("^430",a$icd9_code),
         grep("^431",a$icd9_code),
         grep("^432",a$icd9_code),
         grep("^433",a$icd9_code)),]
code <- b$icd9_code
LD <- diagnoses[which(diagnoses$icd9_code %in% code),]
LD <- LD[,3:4]
colnames(LD) <- c("hadm_id","LD")
LD <- distinct(LD, hadm_id, .keep_all = TRUE)
shai <- left_join(shai,LD,by="hadm_id")
shai$LD[shai$LD>0] <- 1
shai$LD[is.na(shai$LD)] <- 2

#Pneumonia
b <- a[c(grep("^580",a$icd9_code),
         grep("^481",a$icd9_code),
         grep("^482",a$icd9_code),
         grep("^483",a$icd9_code),
         grep("^484",a$icd9_code),
         grep("^485",a$icd9_code),
         grep("^486",a$icd9_code)),]
code <- b$icd9_code
Pneumonia <- diagnoses[which(diagnoses$icd9_code %in% code),]
Pneumonia <- Pneumonia[,3:4]
colnames(Pneumonia) <- c("hadm_id","Pneumonia")
Pneumonia <- distinct(Pneumonia, hadm_id, .keep_all = TRUE)
shai <- left_join(shai,Pneumonia,by="hadm_id")
shai$Pneumonia[shai$Pneumonia>0] <- 1
shai$Pneumonia[is.na(shai$Pneumonia)] <- 2

#Malignancy
Malignancy <- read.csv("Comorbidities/Malignancy.csv")
Malignancy <- Malignancy[,3:4]
colnames(Malignancy) <- c("hadm_id","Malignancy")
Malignancy <- distinct(Malignancy, hadm_id, .keep_all = TRUE)
shai <- left_join(shai,Malignancy,by="hadm_id")
shai$Malignancy[shai$Malignancy>0] <- 1
shai$Malignancy[is.na(shai$Malignancy)] <- 2

#RF
RF <- read.csv("Comorbidities/RF.csv")
RF <- RF[,3:4]
colnames(RF) <- c("hadm_id","RF")
RF <- distinct(RF, hadm_id, .keep_all = TRUE)
shai <- left_join(shai,RF,by="hadm_id")
shai$RF[shai$RF>0] <- 1
shai$RF[is.na(shai$RF)] <- 2


#Bicarbonate
Bicarbonate <- read.csv("Laboratory_tests/Bicarbonate.csv",header=T,na.strings = "NULL")
Bicarbonate <- Bicarbonate[,c(3,5,7)]
colnames(Bicarbonate) <- c("hadm_id","time_Bicarbonate","value_Bicarbonate")
Bicarbonate$hadm_id <- as.integer(Bicarbonate$hadm_id)
df <- inner_join(Bicarbonate,shai,by="hadm_id")
df$time_Bicarbonate <- as.POSIXct(df$time_Bicarbonate)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_Bicarbonate>=(df$in_time-21600) & df$time_Bicarbonate<=(df$in_time+86400),]
df <- df[complete.cases(df$value_Bicarbonate),]
df <- df[order(df$hadm_id,df$time_Bicarbonate),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
Bicarbonate <- as.data.frame(cbind(df$hadm_id,df$value_Bicarbonate))
colnames(Bicarbonate) <- c("hadm_id","value_Bicarbonate")
Bicarbonate$hadm_id <- as.integer(Bicarbonate$hadm_id)
shai <- left_join(shai,Bicarbonate,by="hadm_id")


#Creatinine
Creatinine <- read.csv("Laboratory_tests/Creatinine.csv",header=T,na.strings = "NULL")
Creatinine <- Creatinine[,c(3,5,7)]
colnames(Creatinine) <- c("hadm_id","time_Creatinine","value_Creatinine")
Creatinine$hadm_id <- as.integer(Creatinine$hadm_id)
df <- inner_join(Creatinine,shai,by="hadm_id")
df$time_Creatinine <- as.POSIXct(df$time_Creatinine)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_Creatinine>=(df$in_time-21600) & df$time_Creatinine<=(df$in_time+86400),]
df <- df[complete.cases(df$value_Creatinine),]
df <- df[order(df$hadm_id,df$time_Creatinine),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
Creatinine <- as.data.frame(cbind(df$hadm_id,df$value_Creatinine))
colnames(Creatinine) <- c("hadm_id","value_Creatinine")
Creatinine$hadm_id <- as.integer(Creatinine$hadm_id)
shai <- left_join(shai,Creatinine,by="hadm_id")


#Chloride
Chloride <- read.csv("Laboratory_tests/Chloride.csv",header=T,na.strings = "NULL")
Chloride <- Chloride[,c(3,5,7)]
colnames(Chloride) <- c("hadm_id","time_Chloride","value_Chloride")
Chloride$hadm_id <- as.integer(Chloride$hadm_id)
df <- inner_join(Chloride,shai,by="hadm_id")
df$time_Chloride <- as.POSIXct(df$time_Chloride)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_Chloride>=(df$in_time-21600) & df$time_Chloride<=(df$in_time+86400),]
df <- df[complete.cases(df$value_Chloride),]
df <- df[order(df$hadm_id,df$time_Chloride),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
Chloride <- as.data.frame(cbind(df$hadm_id,df$value_Chloride))
colnames(Chloride) <- c("hadm_id","value_Chloride")
Chloride$hadm_id <- as.integer(Chloride$hadm_id)
shai <- left_join(shai,Chloride,by="hadm_id")


#Glucose
Glucose <- read.csv("Laboratory_tests/Glucose.csv",header=T,na.strings = "NULL")
Glucose <- Glucose[,c(3,5,7)]
colnames(Glucose) <- c("hadm_id","time_Glucose","value_Glucose")
Glucose$hadm_id <- as.integer(Glucose$hadm_id)
df <- inner_join(Glucose,shai,by="hadm_id")
df$time_Glucose <- as.POSIXct(df$time_Glucose)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_Glucose>=(df$in_time-21600) & df$time_Glucose<=(df$in_time+86400),]
df <- df[complete.cases(df$value_Glucose),]
df <- df[order(df$hadm_id,df$time_Glucose),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
Glucose <- as.data.frame(cbind(df$hadm_id,df$value_Glucose))
colnames(Glucose) <- c("hadm_id","value_Glucose")
Glucose$hadm_id <- as.integer(Glucose$hadm_id)
shai <- left_join(shai,Glucose,by="hadm_id")


#Hematocrit
Hematocrit <- read.csv("Laboratory_tests/Hematocrit.csv",header=T,na.strings = "NULL")
Hematocrit <- Hematocrit[,c(3,5,7)]
colnames(Hematocrit) <- c("hadm_id","time_Hematocrit","value_Hematocrit")
Hematocrit$hadm_id <- as.integer(Hematocrit$hadm_id)
df <- inner_join(Hematocrit,shai,by="hadm_id")
df$time_Hematocrit <- as.POSIXct(df$time_Hematocrit)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_Hematocrit>=(df$in_time-21600) & df$time_Hematocrit<=(df$in_time+86400),]
df <- df[complete.cases(df$value_Hematocrit),]
df <- df[order(df$hadm_id,df$time_Hematocrit),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
Hematocrit <- as.data.frame(cbind(df$hadm_id,df$value_Hematocrit))
colnames(Hematocrit) <- c("hadm_id","value_Hematocrit")
Hematocrit$hadm_id <- as.integer(Hematocrit$hadm_id)
shai <- left_join(shai,Hematocrit,by="hadm_id")


#Hemoglobin
Hemoglobin <- read.csv("Laboratory_tests/Hemoglobin.csv",header=T,na.strings = "NULL")
Hemoglobin <- Hemoglobin[,c(3,5,7)]
colnames(Hemoglobin) <- c("hadm_id","time_Hemoglobin","value_Hemoglobin")
Hemoglobin$hadm_id <- as.integer(Hemoglobin$hadm_id)
df <- inner_join(Hemoglobin,shai,by="hadm_id")
df$time_Hemoglobin <- as.POSIXct(df$time_Hemoglobin)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_Hemoglobin>=(df$in_time-21600) & df$time_Hemoglobin<=(df$in_time+86400),]
df <- df[complete.cases(df$value_Hemoglobin),]
df <- df[order(df$hadm_id,df$time_Hemoglobin),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
Hemoglobin <- as.data.frame(cbind(df$hadm_id,df$value_Hemoglobin))
colnames(Hemoglobin) <- c("hadm_id","value_Hemoglobin")
Hemoglobin$hadm_id <- as.integer(Hemoglobin$hadm_id)
shai <- left_join(shai,Hemoglobin,by="hadm_id")


#Platelet
Platelet <- read.csv("Laboratory_tests/Platelet.csv",header=T,na.strings = "NULL")
Platelet <- Platelet[,c(3,5,7)]
colnames(Platelet) <- c("hadm_id","time_Platelet","value_Platelet")
Platelet$hadm_id <- as.integer(Platelet$hadm_id)
df <- inner_join(Platelet,shai,by="hadm_id")
df$time_Platelet <- as.POSIXct(df$time_Platelet)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_Platelet>=(df$in_time-21600) & df$time_Platelet<=(df$in_time+86400),]
df <- df[complete.cases(df$value_Platelet),]
df <- df[order(df$hadm_id,df$time_Platelet),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
Platelet <- as.data.frame(cbind(df$hadm_id,df$value_Platelet))
colnames(Platelet) <- c("hadm_id","value_Platelet")
Platelet$hadm_id <- as.integer(Platelet$hadm_id)
shai <- left_join(shai,Platelet,by="hadm_id")


#Sodium
Sodium <- read.csv("Laboratory_tests/Sodium.csv",header=T,na.strings = "NULL")
Sodium <- Sodium[,c(3,5,7)]
colnames(Sodium) <- c("hadm_id","time_Sodium","value_Sodium")
Sodium$hadm_id <- as.integer(Sodium$hadm_id)
df <- inner_join(Sodium,shai,by="hadm_id")
df$time_Sodium <- as.POSIXct(df$time_Sodium)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_Sodium>=(df$in_time-21600) & df$time_Sodium<=(df$in_time+86400),]
df <- df[complete.cases(df$value_Sodium),]
df <- df[order(df$hadm_id,df$time_Sodium),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
Sodium <- as.data.frame(cbind(df$hadm_id,df$value_Sodium))
colnames(Sodium) <- c("hadm_id","value_Sodium")
Sodium$hadm_id <- as.integer(Sodium$hadm_id)
shai <- left_join(shai,Sodium,by="hadm_id")


#Potassium
Potassium <- read.csv("Laboratory_tests/Potassium.csv",header=T,na.strings = "NULL")
Potassium <- Potassium[,c(3,5,7)]
colnames(Potassium) <- c("hadm_id","time_Potassium","value_Potassium")
Potassium$hadm_id <- as.integer(Potassium$hadm_id)
df <- inner_join(Potassium,shai,by="hadm_id")
df$time_Potassium <- as.POSIXct(df$time_Potassium)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_Potassium>=(df$in_time-21600) & df$time_Potassium<=(df$in_time+86400),]
df <- df[complete.cases(df$value_Potassium),]
df <- df[order(df$hadm_id,df$time_Potassium),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
Potassium <- as.data.frame(cbind(df$hadm_id,df$value_Potassium))
colnames(Potassium) <- c("hadm_id","value_Potassium")
Potassium$hadm_id <- as.integer(Potassium$hadm_id)
shai <- left_join(shai,Potassium,by="hadm_id")


#WBC
WBC <- read.csv("Laboratory_tests/WBC.csv",header=T,na.strings = "NULL")
WBC <- WBC[,c(3,5,7)]
colnames(WBC) <- c("hadm_id","time_WBC","value_WBC")
WBC$hadm_id <- as.integer(WBC$hadm_id)
df <- inner_join(WBC,shai,by="hadm_id")
df$time_WBC <- as.POSIXct(df$time_WBC)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_WBC>=(df$in_time-21600) & df$time_WBC<=(df$in_time+86400),]
df <- df[complete.cases(df$value_WBC),]
df <- df[order(df$hadm_id,df$time_WBC),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
WBC <- as.data.frame(cbind(df$hadm_id,df$value_WBC))
colnames(WBC) <- c("hadm_id","value_WBC")
WBC$hadm_id <- as.integer(WBC$hadm_id)
shai <- left_join(shai,WBC,by="hadm_id")


#PT
PT <- read.csv("Laboratory_tests/PT.csv",header=T,na.strings = "NULL")
PT <- PT[,c(3,5,7)]
colnames(PT) <- c("hadm_id","time_PT","value_PT")
PT$hadm_id <- as.integer(PT$hadm_id)
df <- inner_join(PT,shai,by="hadm_id")
df$time_PT <- as.POSIXct(df$time_PT)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_PT>=(df$in_time-21600) & df$time_PT<=(df$in_time+86400),]
df <- df[complete.cases(df$value_PT),]
df <- df[order(df$hadm_id,df$time_PT),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
PT <- as.data.frame(cbind(df$hadm_id,df$value_PT))
colnames(PT) <- c("hadm_id","value_PT")
PT$hadm_id <- as.integer(PT$hadm_id)
shai <- left_join(shai,PT,by="hadm_id")


#APTT
APTT <- read.csv("Laboratory_tests/APPT.csv",header=T,na.strings = "NULL")
APTT <- APTT[,c(3,5,7)]
colnames(APTT) <- c("hadm_id","time_APTT","value_APTT")
APTT$hadm_id <- as.integer(APTT$hadm_id)
df <- inner_join(APTT,shai,by="hadm_id")
df$time_APTT <- as.POSIXct(df$time_APTT)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_APTT>=(df$in_time-21600) & df$time_APTT<=(df$in_time+86400),]
df <- df[complete.cases(df$value_APTT),]
df <- df[order(df$hadm_id,df$time_APTT),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
APTT <- as.data.frame(cbind(df$hadm_id,df$value_APTT))
colnames(APTT) <- c("hadm_id","value_APTT")
APTT$hadm_id <- as.integer(APTT$hadm_id)
shai <- left_join(shai,APTT,by="hadm_id")


#INR
INR <- read.csv("Laboratory_tests/INR.csv",header=T,na.strings = "NULL")
INR <- INR[,c(3,5,7)]
colnames(INR) <- c("hadm_id","time_INR","value_INR")
INR$hadm_id <- as.integer(INR$hadm_id)
df <- inner_join(INR,shai,by="hadm_id")
df$time_INR <- as.POSIXct(df$time_INR)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_INR>=(df$in_time-21600) & df$time_INR<=(df$in_time+86400),]
df <- df[complete.cases(df$value_INR),]
df <- df[order(df$hadm_id,df$time_INR),]
df <- distinct(df, hadm_id, .keep_all = TRUE)
INR <- as.data.frame(cbind(df$hadm_id,df$value_INR))
colnames(INR) <- c("hadm_id","value_INR")
INR$hadm_id <- as.integer(INR$hadm_id)
shai <- left_join(shai,INR,by="hadm_id")


#SOFA
SOFA <- read.csv("ICU_admission/sofa.csv")
SOFA <- SOFA[,c(3,4)]
shai <- left_join(shai,SOFA,by="icustay_id")


#SAPSII
SAPSII <- read.csv("ICU_admission/sapsii.csv")
SAPSII <- SAPSII[,c(3,4)]
shai <- left_join(shai,SAPSII,by="icustay_id")


#30-day mortality
df <- shai
#Get rid of the NA and keep only the dead
df <- df[complete.cases(df$dod),]
df$death_time <- as.POSIXct(df$dod)
df$admit_out <- as.POSIXct(df$admittime)
df <- df[df$death_time <= (df$admit_out+2592000),]
df$day30 <- difftime(df$death_time,df$admittime,units = "days")
df <- separate(data = df, col = day30, into = c("day30", "day2"), sep = " ")
class(df$day30)
df$day30 <- as.numeric(df$day30)
#取hadm_id和day30
df <- df[,c(1,54)]
colnames(df) <- c("hadm_id","day30_mortality")
shai <- left_join(shai,df,by="hadm_id")


#90-day mortality
df <- shai
df <- df[complete.cases(df$dod),]
df$death_time <- as.POSIXct(df$dod)
df$admit_out <- as.POSIXct(df$admittime)
df <- df[df$death_time <= (df$admit_out+7776000),]
df$day90 <- difftime(df$death_time,df$admittime,units = "days")
df <- separate(data = df, col = day90, into = c("day90", "day2"), sep = " ")
class(df$day90)
df$day90 <- as.numeric(df$day90)
df <- df[,c(1,55)]
colnames(df) <- c("hadm_id","day90_mortality")
df$hadm_id <- as.integer(df$hadm_id)
shai <- left_join(shai,df,by="hadm_id")


#365-day mortality
df <- shai
df <- df[complete.cases(df$dod),]
df$death_time <- as.POSIXct(df$dod)
df$admit_out <- as.POSIXct(df$admittime)
df <- df[df$death_time <= (df$admit_out+31536000),]
df$day365 <- difftime(df$death_time,df$admittime,units = "days")
df <- separate(data = df, col = day365, into = c("day365", "day2"), sep = " ")
class(df$day365)
df$day365 <- as.numeric(df$day365)
df <- df[,c(1,56)]
colnames(df) <- c("hadm_id","day365_mortality")
df$hadm_id <- as.integer(df$hadm_id)
shai <- left_join(shai,df,by="hadm_id")


#存档
write.csv(shai,"wu_0920.csv")

##############################
library(tidyverse)
shai <- read.csv("wu_0920.csv")
#Delete the unwanted index
shai <- shai[,-c(1,3,5,7,9:11,14,16,18:21)]
#Save the mortality data. You can't interpolate this
a <- shai[,c(1,41:43)]
#Delete the mortality rate
shai <- shai[,-c(41:43)]
#Test for variables that are missing more than 5% of patients and for variables that are missing more than half of the data
pMiss <- function(x){round(sum(is.na(x))/length(x),3)}
shai$aaa <- apply(shai, 1, pMiss)
table(shai$aaa>0.05) #295 patients were missing more than 5%
shai <- shai[shai$aaa<=0.05,]
apply(shai, 2, pMiss)
# value_Hemoglobin
# 0.619
#Delete Hemoglobin
shai <- shai[,-31]

#backup
data <- shai
library(mice)
library(VIM)
#View missing percentage
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#Multiple interpolation
tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
densityplot(tempData)
#In theory, you should look at the best fit for each metric, but I chose Temperature, which has the most missing points
densityplot(tempData,~ value_Temperature | .imp == 1)
#Choose the best 1
completedData <- complete(tempData,action = 1)

#Use interpolation data
data <- completedData
#Divide the races
data$ethnicity <- ifelse(!str_detect(data$ethnicity,"WHITE")
                         ,ifelse(str_detect(data$ethnicity,"BLACK"), 'black','other'), 'white')
table(data$ethnicity)

#aaa forgot to delete it, so let's delete it here
data <- data[,-40]
#Plus death figures
data <- inner_join(data,a,by="hadm_id")

########################
#Let's change the race to 123
data$ethnicity[which(data$ethnicity=="white")] <- 1
data$ethnicity[which(data$ethnicity=="black")] <- 2
data$ethnicity[which(data$ethnicity=="other")] <- 3
#Change the gender to 12
data$gender[which(data$gender=="M")] <- 1
data$gender[which(data$gender=="F")] <- 2

#Add live state
data$status_day30 <- ifelse(data$day30_mortality<=30,2,1)
data$status_day30[is.na (data$status_day30)] <- 1
data$day30_mortality[is.na(data$day30_mortality)] <- 30

data$status_day90 <- ifelse(data$day90_mortality<=90,2,1)
data$status_day90[is.na (data$status_day90)] <- 1
data$day90_mortality[is.na(data$day90_mortality)] <- 90

data$status_day365 <- ifelse(data$day365_mortality<=365,2,1)
data$status_day365[is.na (data$status_day365)] <- 1
data$day365_mortality[is.na(data$day365_mortality)] <- 365

write.csv(data,"data.csv")

