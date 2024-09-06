{library(readxl)
  library(car)
  library(nlme)
  library(MASS)
  library(tidyverse)
  library(rcompanion)
  library(lme4)
  library(ggplot2)
  library(ggpubr)
  library(bestNormalize)
  library(multcomp)}
######################################################
##########  S E E D L I N G      M A S S
###########
#####################################################
data<-read_xlsx("Seedling_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
##Seedling mass
###Location ~ SSM
#No transformation
modelSB<- lm(SSM~Location,data=SB)
modelSB
Anova(modelSB, type="II")
xSB = (residuals(modelSB))
xSB
#Square root transformation
sqtransSB = sqrt(SB$SSM)
modelSB2<-lm(sqtransSB~Location, data=SB)
Anova(modelSB2, type="II")
xSB2 = (residuals(modelSB2))
xSB2
#Log transformation
logtransSB = log(SB$SSM)
modelSB3<-lm(logtransSB~Location, data=SB)
Anova(modelSB3, type="II") 
xSB3 = (residuals(modelSB3))
xSB3

#Cube root transformation
T_cubSB = sign(SB$SSM) * abs(SB$SSM)^(1/3)
modelSB4<-lm(T_cubSB~Location, data=SB)
Anova(modelSB4, type="II")
xSB4 = (residuals(modelSB4))
xSB4
#Squaring transformation
squaringSB = (SB$SSM)^2
modelSB5<-lm(squaringSB~Location, data=SB)
######CORRECTED MODEL B5
Anova(modelSB5, type="II") ##### THIS IS THE ONE
xSB5 = (residuals(modelSB5))
xSB5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xSB)   #NT
shapiro.test(xSB2)  #Sqrt
shapiro.test(xSB3)  #Log
shapiro.test(xSB4)  #Cubert
shapiro.test(xSB5)  #Squaring

leveneTest(xSB~Location, SB) #NT
leveneTest(xSB2~Location, SB) #Sqrt
leveneTest(xSB3~Location, SB) #Log
leveneTest(xSB4~Location, SB) #Cubert
leveneTest(xSB5~Location, SB) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xSB)
plotNormalHistogram(xSB2)
plotNormalHistogram(xSB3)
plotNormalHistogram(xSB4)
plotNormalHistogram(xSB5)

qqnorm(residuals(modelSB),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSB),
       col="red")

qqnorm(residuals(modelSB2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSB2),
       col="red")

qqnorm(residuals(modelSB3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSB3),
       col="red")

qqnorm(residuals(modelSB4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSB4),
       col="red")

qqnorm(residuals(modelSB5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSB5),
       col="red")

plot(fitted(modelSB),
     residuals(modelSB))

plot(fitted(modelSB2),
     residuals(modelSB2))

plot(fitted(modelSB3),
     residuals(modelSB3))

plot(fitted(modelSB4),
     residuals(modelSB4))


plot(fitted(modelSB5),
     residuals(modelSB5))
TUKEYSB<- summary(glht(modelSB5, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYSB
# Assuming TUKEYSB is your summary object
significant_pairs <- sum(TUKEYSB$test$pvalues < 0.05)
total_pairs <- length(TUKEYSB$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")


tSB<- glht(modelSB3, linfct = mcp(location= "Tukey"))
t.cldSB <- cld(tSB)   # letter-based display
t.cldSB

#What is the effect of Euclidean distance on seedling weight in Stipa borysthenica? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

SX=read_xlsx("Seedling_Masters.xlsx") #### OPEN OUR CLEANED DATAFRAME
SY=SX%>% 
  filter(Species=="Stipa borysthenica")
SZ=SY%>% 
  group_by(Location)%>% 
  summarize(
    avg_SSM=mean(SSM),
    avg_SLH=mean(SLH))

write.csv(SZ, "Festuca_meanSSM_SLH.csv")

#What is the effect of geographical distance (Km) on seedling weight in Stipa borysthenica? 
#Open file
SD<-read_xlsx("SB_Distance_SSM.xlsx")
SD
#Best transformation
#No transformation
modelSD <- lm(Absolute~DISTANCE_KM , data=SD)
summary(modelSD)
Anova(modelSD, type="II")
xSD = (residuals(modelSD))
xSD
#Square root transformation
sqtransSD = sqrt(SD$Absolute)
modelSD2<-lm(sqtransSD~DISTANCE_KM, data=SD)
Anova(modelSD2, type="II") #THIS IS THE ONE
xSD2 = (residuals(modelSD2))
xSD2
#Log transformation
logtransSD = log(SD$Absolute)
modelSD3<-lm(logtransSD~DISTANCE_KM,  data=SD)
Anova(modelSD3, type="II")
xSD3 = (residuals(modelSD3))
xSD3
#Cube root transformation
T_cubSD = sign(SD$Absolute) * abs(SD$Absolute)^(1/3)
modelSD4<-lm(T_cubSD~DISTANCE_KM,data=SD)
Anova(modelSD4, type="II")    
xSD4 = (residuals(modelSD4))
xSD4
#Squaring transformation
squaringSD = (SD$Absolute)^2
modelSD5<-lm(squaringSD~DISTANCE_KM,  data=SD)
#### CORRECTED MODEL SD5
Anova(modelSD5, type="II") ### THIS iS THE ONE
xSD5 = (residuals(modelSD5))
xSD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xSD)   #NT
shapiro.test(xSD2)  #Sqrt
shapiro.test(xSD3)  #Log
shapiro.test(xSD4)  #Cubert
shapiro.test(xSD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xSD)
plotNormalHistogram(xSD2)
plotNormalHistogram(xSD4)
plotNormalHistogram(xSD5)

qqnorm(residuals(modelSD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSD),
       col="red")

qqnorm(residuals(modelSD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSD2),
       col="red")

qqnorm(residuals(modelSD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSD4),
       col="red")

qqnorm(residuals(modelSD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSD5),
       col="red")


plot(fitted(modelSD),
     residuals(modelSD))

plot(fitted(modelSD2),
     residuals(modelSD2))

plot(fitted(modelSD4),
     residuals(modelSD4))

plot(fitted(modelSD5),
     residuals(modelSD5))


#Distance- Absolute weight relationship plotting
##regression
plot(SD$DISTANCE_KM,T_cubSD, main="Connection between distance among locations on C.arenaria Seedling weight",
     ylab="Seedling weight difference betweeen locations (FRT) (g)", xlab="Distance (km)")
abline(lm(T_cubSD~SD$DISTANCE_KM), col="red") # regression line (y~x)
T_cubSD.lm = lm(T_cubSD~SD$DISTANCE_KM, data=SD)
summary(T_cubSD.lm) 

#What is the effect of Ecological distance (STZ) on seedling weight in Stipa borysthenica? 
#Best transformation
#No transformation
data<-read_xlsx("Seedling_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)

modelSZ<- lme(SSM~STZ,random = ~ 1 | Location,data=SB)
modelSZ
Anova(modelSZ, type="II")
xSZ = (residuals(modelSZ))
xSZ
#Square root transformation
sqtransSZ = sqrt(SB$SSM)
modelSZ2<-lme(sqtransSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSZ2, type="II")
xSZ2 = (residuals(modelSZ2))
xSZ2
#Log transformation
logtransSZ = log(SB$SSM)
modelSZ3<-lme(logtransSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSZ3, type="II") 
xSZ3 = (residuals(modelSZ3))
xSZ3
#Cube root transformation
T_cubSZ = sign(SB$SSM) * abs(SB$SSM)^(1/3)
modelSZ4<-lme(T_cubSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSZ4, type="II")
xSZ4 = (residuals(modelSZ4))
xSZ4
#Squaring transformation
squaringSZ = (SB$SSM)^2
modelSB5<-lm(squaringSZ~STZ,random = ~ 1 | Location, data=SB)
###CORRECTED MODEL SB5
Anova(modelSB5, type="II") #THIS IS THE ONE
xSZ5 = (residuals(modelSB5))
xSZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xSZ)   #NT
shapiro.test(xSZ2)  #Sqrt
shapiro.test(xSZ3)  #Log
shapiro.test(xSZ4)  #Cubert
shapiro.test(xSZ5)  #Squaring
leveneTest(xSZ~STZ, SB) #NT
leveneTest(xSZ2~STZ, SB) #Sqrt
leveneTest(xSZ3~STZ, SB) #Log
leveneTest(xSZ4~STZ, SB) #Cubert
leveneTest(xSZ5~STZ, SB) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xSZ)
plotNormalHistogram(xSZ2)
plotNormalHistogram(xSZ3)
plotNormalHistogram(xSZ4)
plotNormalHistogram(xSZ5)
skewness(xSZ3)
kurtosis(xSZ3)
qqnorm(residuals(modelSZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSZ3),
       col="red")
plot(fitted(modelSZ3),
     residuals(modelSZ))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on seedling weight in Stipa borysthenica? 
#Best transformation
#No transformation
data<-read_xlsx("Seedling_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
modelFR <- lme(SSM~Aridity, random = ~ 1 | Location, data=SB)
summary(modelFR)
Anova(modelFR, type="II")
xFR = (residuals(modelFR))
xFR
#Square root transformation
sqtransFR = sqrt(SB$SSM)
modelFR2<-lme(sqtransFR~Aridity,random = ~ 1 | Location, data=SB)
Anova(modelFR2, type="II")
xFR2 = (residuals(modelFR2))
xFR2
#Log transformation
logtransFR = log(SB$SSM)
modelFR3<-lme(logtransFR~Aridity, random = ~ 1 | Location, data=SB)
Anova(modelFR3, type="II") 
xFR3 = (residuals(modelFR3))
xFR3
#Cube root transformation
T_cubFR = sign(SB$SSM) * abs(SB$SSM)^(1/3)
modelFR4<-lme(T_cubFR~Aridity,random = ~ 1 | Location, data=SB)
Anova(modelFR4, type="II")
xFR4 = (residuals(modelFR4))
xFR4
#Squaring transformation
squaringFR = (SB$SSM)^2
modelFR5<-lme(squaringFR~Aridity, random = ~ 1 | Location, data=SB)
#### CORRECTED MODEL FR5
Anova(modelFR5, type="II") #THIS IS THE ONE
xFR5 = (residuals(modelFR5))
xFR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFR)   #NT
shapiro.test(xFR2)  #Sqrt
shapiro.test(xFR3)  #Log
shapiro.test(xFR4)  #Cubert
shapiro.test(xFR5)  #Squaring
bartlett.test(SB$weight, SB$aridity)  #NT
bartlett.test(sqtransFR, SB$aridity)  #Sqrt
bartlett.test(logtransFR, SB$aridity) #Log
bartlett.test(T_cubFR, SB$aridity)    #Cubert
bartlett.test(squaringFR, SB$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xFR3)
skewness(xFR3)
kurtosis(xFR3)
qqnorm(residuals(modelFR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFR3),
       col="red")
plot(fitted(modelFR3),
     residuals(modelFR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(SB, aes(x = Aridity, y = logtransFR, colour=STZ)) +
  labs(x="Aridity index",y="Seedling weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelFR3)))) 
#

##################################################################
################### SEEDLING SIZE ANALYSIS (SLH)
##################
##################################################################

data<-read_xlsx("Seedling_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
##Seedling maximum leaf height
###Location ~ SLH
#No transformation
modelSB<- lm(SLH~Location,data=SB)
modelSB
Anova(modelSB, type="II")
xSB = (residuals(modelSB))
xSB
#Square root transformation
sqtransSB = sqrt(SB$SLH)
modelSB2<-lm(sqtransSB~Location, data=SB)
Anova(modelSB2, type="II")
xSB2 = (residuals(modelSB2))
xSB2
#Log transformation
logtransSB = log(SB$SLH)
modelSB3<-lm(logtransSB~Location, data=SB)
Anova(modelSB3, type="II")  
xSB3 = (residuals(modelSB3))
xSB3

#Cube root transformation
T_cubSB = sign(SB$SLH) * abs(SB$SLH)^(1/3)
modelSB4<-lm(T_cubSB~Location, data=SB)
Anova(modelSB4, type="II") 
xSB4 = (residuals(modelSB4))
xSB4
#Squaring transformation
squaringSB = (SB$SLH)^2
modelSB5<-lm(squaringSB~Location, data=SB)
###CORRECTED MODEL SB5
Anova(modelSB5, type="II") ####### THIS IS THE ONE
xSB5 = (residuals(modelSB5))
xSB5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xSB)   #NT
shapiro.test(xSB2)  #Sqrt
shapiro.test(xSB3)  #Log
shapiro.test(xSB4)  #Cubert
shapiro.test(xSB5)  #Squaring

leveneTest(xSB~Location, SB) #NT
leveneTest(xSB2~Location, SB) #Sqrt
leveneTest(xSB3~Location, SB) #Log
leveneTest(xSB4~Location, SB) #Cubert
leveneTest(xSB5~Location, SB) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xSB)
plotNormalHistogram(xSB2)
plotNormalHistogram(xSB3)
plotNormalHistogram(xSB4)
plotNormalHistogram(xSB5)

qqnorm(residuals(modelSB),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSB),
       col="red")

qqnorm(residuals(modelSB2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSB2),
       col="red")

qqnorm(residuals(modelSB3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSB3),
       col="red")

qqnorm(residuals(modelSB4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSB4),
       col="red")

qqnorm(residuals(modelSB5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSB5),
       col="red")

plot(fitted(modelSB),
     residuals(modelSB))

plot(fitted(modelSB2),
     residuals(modelSB2))

plot(fitted(modelSB3),
     residuals(modelSB3))

plot(fitted(modelSB4),
     residuals(modelSB4))


plot(fitted(modelSB5),
     residuals(modelSB5))
TUKEYSB<- summary(glht(modelSB5, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYSB
# Assuming TUKEYSB is your summary object
significant_pairs <- sum(TUKEYSB$test$pvalues < 0.05)
total_pairs <- length(TUKEYSB$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")
tSB<- glht(modelSB3, linfct = mcp(location= "Tukey"))
t.cldSB <- cld(tSB)   # letter-based display
t.cldSB

#What is the effect of Ecological distance (STZ) on seedling weight in Stipa borysthenica? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

CX=read_xlsx("Seedling_Masters.xlsx") #### OPEN OUR CLEANED DATAFRAME
CY=CX%>% 
  filter(Species=="Stipa borysthenica")
SZ=CY%>% 
  group_by(Location)%>% 
  summarize(
    avg_SLH=mean(SLH),
    avg_SLH=mean(SLH))

#write.csv(SZ, "meanSLH_SLH.csv")

#What is the effect of geographical distance (Km) on seedling weight in Stipa borysthenica? 
#Open file
SD<-read_xlsx("SB_Distance_SLH.xlsx")
SD
#Best transformation
#No transformation
modelSD <- lm(Absolute~DISTANCE_KM , data=SD)
summary(modelSD)
Anova(modelSD, type="II")
xSD = (residuals(modelSD))
xSD
#Square root transformation
sqtransSD = sqrt(SD$Absolute)
modelSD2<-lm(sqtransSD~DISTANCE_KM, data=SD)
Anova(modelSD2, type="II") ### THIS iS THE ONE
xSD2 = (residuals(modelSD2))
xSD2
#Log transformation
logtransSD = log(SD$Absolute)
modelSD3<-lm(logtransSD~DISTANCE_KM,  data=SD)
Anova(modelSD3, type="II")
xSD3 = (residuals(modelSD3))
xSD3
#Cube root transformation
T_cubSD = sign(SD$Absolute) * abs(SD$Absolute)^(1/3)
modelSD4<-lm(T_cubSD~DISTANCE_KM,data=SD)
Anova(modelSD4, type="II")    
xSD4 = (residuals(modelSD4))
xSD4
#Squaring transformation
squaringSD = (SD$Absolute)^2
modelSD5<-lm(squaringSD~DISTANCE_KM,  data=SD)
### CORRECTED MODEL SD5
Anova(modelSD5, type="II")
xSD5 = (residuals(modelSD5))
xSD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xSD)   #NT
shapiro.test(xSD2)  #Sqrt
shapiro.test(xSD3)  #Log
shapiro.test(xSD4)  #Cubert
shapiro.test(xSD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xSD)
plotNormalHistogram(xSD2)
plotNormalHistogram(xSD4)
plotNormalHistogram(xSD5)

qqnorm(residuals(modelSD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSD),
       col="red")

qqnorm(residuals(modelSD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSD2),
       col="red")

qqnorm(residuals(modelSD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSD4),
       col="red")

qqnorm(residuals(modelSD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSD5),
       col="red")


plot(fitted(modelSD),
     residuals(modelSD))

plot(fitted(modelSD2),
     residuals(modelSD2))

plot(fitted(modelSD4),
     residuals(modelSD4))

plot(fitted(modelSD5),
     residuals(modelSD5))


#Distance- Absolute weight relationship plotting
##regression
plot(SD$DISTANCE_KM,T_cubSD, main="Connection between distance among locations on C.arenaria Seedling weight",
     ylab="Seedling weight difference betweeen locations (FRT) (g)", xlab="Distance (km)")
abline(lm(T_cubSD~SD$DISTANCE_KM), col="red") # regression line (y~x)
T_cubSD.lm = lm(T_cubSD~SD$DISTANCE_KM, data=SD)
summary(T_cubSD.lm) 

#What is the effect of Ecological distance (STZ) on seedling weight in Stipa borysthenica? 
#Best transformation
#No transformation
data<-read_xlsx("Seedling_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
SB$STZ=as.factor(SB$STZ)
modelSZ<- lme(SLH~STZ,random = ~ 1 | Location,data=SB)
modelSZ
Anova(modelSZ, type="II")
xSZ = (residuals(modelSZ))
xSZ
#Square root transformation
sqtransSZ = sqrt(SB$SLH)
modelSZ2<-lme(sqtransSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSZ2, type="II")
xSZ2 = (residuals(modelSZ2))
xSZ2
#Log transformation
logtransSZ = log(SB$SLH)
modelSZ3<-lme(logtransSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSZ3, type="II") 
xSZ3 = (residuals(modelSZ3))
xSZ3
#Cube root transformation
T_cubSZ = sign(SB$SLH) * abs(SB$SLH)^(1/3)
modelSZ4<-lme(T_cubSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSZ4, type="II") 
xSZ4 = (residuals(modelSZ4))
xSZ4
#Squaring transformation
squaringSZ = (SB$SLH)^2
modelSB5<-lme(squaringSZ~STZ,random = ~ 1 | Location, data=SB)
####CORRECTED MODEL SB5
Anova(modelSB5, type="II") ### THIS IS THE ONE
xSZ5 = (residuals(modelSB5))
xSZ5

# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xSZ)   #NT
shapiro.test(xSZ2)  #Sqrt
shapiro.test(xSZ3)  #Log
shapiro.test(xSZ4)  #Cubert
shapiro.test(xSZ5)  #Squaring
leveneTest(xSZ~STZ, SB) #NT
leveneTest(xSZ2~STZ, SB) #Sqrt
leveneTest(xSZ3~STZ, SB) #Log
leveneTest(xSZ4~STZ, SB) #Cubert
leveneTest(xSZ5~STZ, SB) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xSZ3)
skewness(xSZ3)
kurtosis(xSZ3)
qqnorm(residuals(modelSZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSZ3),
       col="red")
plot(fitted(modelSZ3),
     residuals(modelSZ))
TUKEYSZ<- summary(glht(modelSB5, linfct = mcp(STZ = "Tukey")), test = adjusted("holm"))
TUKEYSZ
# Assuming TUKEYSB is your summary object
significant_pairs <- sum(TUKEYSZ$test$pvalues < 0.05)
total_pairs <- length(TUKEYSZ$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")
#tSB<- glht(modelSB3, linfct = mcp(location= "Tukey"))
#t.cldSB <- cld(tSB)   # letter-based display
#t.cldSB

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on seedling longest leaf in Stipa borysthenica? 
#Best transformation
#No transformation
data<-read_xlsx("Seedling_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
modelFR <- lme(SLH~Aridity, random = ~ 1 | Location, data=SB)
summary(modelFR)
Anova(modelFR, type="II")
xFR = (residuals(modelFR))
xFR
#Square root transformation
sqtransFR = sqrt(SB$SLH)
modelFR2<-lme(sqtransFR~Aridity,random = ~ 1 | Location, data=SB)
Anova(modelFR2, type="II")
xFR2 = (residuals(modelFR2))
xFR2
#Log transformation
logtransFR = log(SB$SLH)
modelFR3<-lme(logtransFR~Aridity, random = ~ 1 | Location, data=SB)
Anova(modelFR3, type="II") 
xFR3 = (residuals(modelFR3))
xFR3
#Cube root transformation
T_cubFR = sign(SB$SLH) * abs(SB$SLH)^(1/3)
modelFR4<-lme(T_cubFR~Aridity,random = ~ 1 | Location, data=SB)
Anova(modelFR4, type="II") 
xFR4 = (residuals(modelFR4))
xFR4
#Squaring transformation
squaringFR = (SB$SLH)^2
modelFR5<-lme(squaringFR~Aridity, random = ~ 1 | Location, data=SB)
###CORRECTED MODEL FR5
Anova(modelFR5, type="II") ### THIS IS THE ONE
xFR5 = (residuals(modelFR5))
xFR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFR)   #NT
shapiro.test(xFR2)  #Sqrt
shapiro.test(xFR3)  #Log
shapiro.test(xFR4)  #Cubert
shapiro.test(xFR5)  #Squaring
bartlett.test(SB$weight, SB$aridity)  #NT
bartlett.test(sqtransFR, SB$aridity)  #Sqrt
bartlett.test(logtransFR, SB$aridity) #Log
bartlett.test(T_cubFR, SB$aridity)    #Cubert
bartlett.test(squaringFR, SB$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xFR3)
skewness(xFR3)
kurtosis(xFR3)
qqnorm(residuals(modelFR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFR3),
       col="red")
plot(fitted(modelFR3),
     residuals(modelFR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(SB, aes(x = Aridity, y = logtransFR, colour=STZ)) +
  labs(x="Aridity index",y="Seedling weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelFR3)))) 
#

