install.packages("tidyverse")
install.packages("jsonlite")
setwd("~/Desktop/14R/R-Analysis")
setwd("~/Desktop/14R/R-Analysis/MechaCarChallenge.RScript")
mechaCar <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F) #read in dataset
head(mechaCar)

#MPG Regression
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance, data=mechaCar) #generate multiple linear regression model
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance, data=mechaCar))
lm(mpg ~ vehicle.length,mechaCar)
summary(lm(mpg ~ vehicle.length,mechaCar))

#Suspension Coil Summary
sus_coil <- read.csv('Suspension_Coil.csv',stringsAsFactors = F) #read in dataset
head(sus_coil)
library(tidyverse)
PSI <- unlist(sus_coil["PSI"])
lot_summary <- lot_summary %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))
view(lot_summary) 


#SUS_Coil t-tests

lot1 <- t.test(subset(sus_coil,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
view(lot1)

lot2 <- t.test(subset(sus_coil,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
view(lot2)

lot3 <- t.test(subset(sus_coil,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)
view(lot3)

lot_all <- t.test(sus_coil$PSI,mu = 1500)
view(lot_all)