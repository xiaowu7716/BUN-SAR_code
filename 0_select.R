rm(list = ls()) #One-click clear
library(tidyverse)  #Load universal package
#Read the required file
patients <- read.csv("patients.csv",header=T,na.strings = "NULL")
admissions <- read.csv("admissions.csv",header=T,na.strings = "NULL")
icustays <- read.csv("icustays.csv",header=T,na.strings = "NULL")
#Delete unnecessary data
icustays <- icustays[,c(3:4,10:12)]
admissions <- admissions[,c(2:6,14)]
patients <- patients[,c(2:5)]
df1 <- inner_join(admissions,icustays,by="hadm_id")

#AKI     33352
AKI <- read.csv("AKI.csv")#AKI data
AKI <- AKI[AKI$aki_stage>0,]#Screening AKI1, 2,3
#For code reasons, the same person may have three stages of AKI during the screening process
#So pick the heaviest stage
AKI <- arrange(AKI,icustay_id,desc(aki_stage))
#Pick the first one
AKI <- distinct(AKI, icustay_id, .keep_all = TRUE)
df <- inner_join(df1,AKI,by="icustay_id")

#BUN   31067
BUN <- read.csv("BUN.csv",header=T,na.strings = "NULL")
BUN <- BUN[,c(3,5,7)]
colnames(BUN) <- c("hadm_id","time_BUN","value_BUN")
BUN$hadm_id <- as.integer(BUN$hadm_id)
df <- inner_join(BUN,df,by="hadm_id")
#Convert to time format
df$time_BUN <- as.POSIXct(df$time_BUN)
df$in_time <- as.POSIXct(df$intime)
#The time was six hours before admission to the icu and 24 hours after admission
df<- df[df$time_BUN >= (df$in_time-21600) & df$time_BUN <= (df$in_time+86400),]
df$value_BUN[df$value_BUN == 0] <- NA
df <- df[complete.cases(df$value_BUN),]
df <- df[order(df$hadm_id,df$time_BUN),]
df <- distinct(df, hadm_id, .keep_all = TRUE)

#Albumin     11935
Albumin <- read.csv("Albumin.csv",header=T,na.strings = "NULL")
Albumin <- Albumin[,c(3,5,7)]
colnames(Albumin) <- c("hadm_id","time_Albumin","value_Albumin")
Albumin$hadm_id <- as.integer(Albumin$hadm_id)
df <- inner_join(Albumin,df,by="hadm_id")
df$time_Albumin <- as.POSIXct(df$time_Albumin)
df$in_time <- as.POSIXct(df$intime)
df <- df[df$time_Albumin >= (df$in_time-21600) & df$time_Albumin <= (df$in_time+86400),]
df$value_Albumin[df$value_Albumin == 0] <- NA
df <- df[complete.cases(df$value_Albumin),]
df <- df[order(df$hadm_id,df$time_Albumin),]
df <- distinct(df, hadm_id, .keep_all = TRUE)

#成人    11262
#patients are connected by subject_id
data <- inner_join(df,patients,by="subject_id")
#Age was calculated using admission data and birth data
data$day <- difftime(data$admittime,data$dob,units = "days")
#Date and time is not easy to calculate the age, so export it to excel and use the YEAR function to do it
data <- separate(data = data, col = day, into = c("day1", "day2"), sep = " ")

data$day1 <- as.numeric(data$day1)
data$age <- data$day1/365
data <- data[data$age>=18&data$age<299,]

#First admission to icu     8735
#Stay in ICU between admission and discharge
data <- data[complete.cases(data$outtime),]
data$icu_in <- as.POSIXct(data$intime)
data$icu_out <- as.POSIXct(data$outtime)
data$admit_in <- as.POSIXct(data$admittime)
data$admit_out <- as.POSIXct(data$dischtime)
data <- data[data$icu_in>=data$admit_in & data$icu_out<=data$admit_out,]
#First admission to ICU
#Sort first, then keep the first duplicate
data <- data[order(data$hadm_id,data$intime),]
data <- distinct(data, hadm_id, .keep_all = TRUE)


#reserve   6254
df <- data
table(df$los>=2)
df <- df[df$los>=2,]
df$BUNAR <- df$value_BUN/df$value_Albumin
#Delete useless data
df <- df[,-c(6,8:9,15:21,25,27:28)]

write.csv(df,"select.csv")



