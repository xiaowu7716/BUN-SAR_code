rm(list= ls())
library(ggplot2)
library(pROC)
library(tidyverse)
source("ROC_Plot.R")
source("Theme_Publication.R")

data <- read.csv("../1_data/data.csv")
x1 <- data$BUNAR
x2 <- data$value_BUN
x3 <- data$value_Albumin
x4 <- data$sapsii
y <- data$status_day30

R1 <- roc(y,x1,ci = T); R2 <- roc(y,x2,ci = T); R3 <- roc(y,x3,ci = T);R4 <- roc(y,x4,ci = T)


dat1 <- data.frame(Sensitivities = R1$sensitivities, 
                   FalsePositiveRate = (1-R1$specificities), 
                   Model = rep("BUN-Albumin ratio",length(R1$sensitivities)))
dat2 <- data.frame(Sensitivities = R2$sensitivities, 
                   FalsePositiveRate = (1-R2$specificities), 
                   Model = rep("BUN",length(R2$sensitivities)))
dat3 <- data.frame(Sensitivities = R3$sensitivities, 
                   FalsePositiveRate = (1-R3$specificities), 
                   Model = rep("Albumin",length(R3$sensitivities)))
dat4 <- data.frame(Sensitivities = R4$sensitivities, 
                   FalsePositiveRate = (1-R4$specificities), 
                   Model = rep("sapsii",length(R4$sensitivities)))

dat <- rbind(dat1,dat2,dat3,dat4)
colnames(dat)[2] <- ("1-Specificities")



ggplot(dat,aes(`1-Specificities`,Sensitivities,colour = Model)) + 
  geom_roc_plot()+
  scale_colour_Publication() + 
  theme_bw()+
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  theme(panel.border = element_rect(colour = 'black'),
  legend.title = element_blank(),legend.position = c(0.8,0.2),
  legend.direction = 'vertical',panel.grid=element_blank())

#Table making
mir <- (c('BUN-Albumin ratio','BUN','Albumin','sapsii'))
auc <- data.frame(rn= c('low','auc','up'))
roc <- list(R1,R2,R3,R4)

for (n in 1:4) {
  auc1 <- data.frame(roc[[n]]$ci)
  auc <- cbind(auc,auc1)
}
rownames(auc) <- auc[,1]
auc <- auc[,-1]
colnames(auc) <- mir
write.csv(auc,'auc_30.csv')
