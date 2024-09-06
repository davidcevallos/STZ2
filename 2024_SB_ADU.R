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
##########  S H O O T      M A S S
###########
#####################################################
data<-read_xlsx("Adult_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
##Adult mass
###Location ~ shoot
#No transformation
modelSB<- lm(shoot~Location,data=SB)
modelSB
Anova(modelSB, type="II")
xSB = (residuals(modelSB))
xSB
#Square root transformation
sqtransSB = sqrt(SB$shoot)
modelSB2<-lm(sqtransSB~Location, data=SB)
Anova(modelSB2, type="II")
xSB2 = (residuals(modelSB2))
xSB2
#Log transformation
logtransSB = log(SB$shoot)
modelSB3<-lm(logtransSB~Location, data=SB)
Anova(modelSB3, type="II") ##### THIS IS THE ONE
xSB3 = (residuals(modelSB3))
xSB3

#Cube root transformation
T_cubSB = sign(SB$shoot) * abs(SB$shoot)^(1/3)
modelSB4<-lm(T_cubSB~Location, data=SB)
Anova(modelSB4, type="II") 
xSB4 = (residuals(modelSB4))
xSB4
#Squaring transformation
squaringSB = (SB$shoot)^2
modelSB5<-lm(squaringSB~Location, data=SB)
#CORRECTED SB5
Anova(modelSB5, type="II") 
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
TUKEYSB<- summary(glht(modelSB3, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
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

#What is the effect of Euclidean distance on Adult weight in Stipa borysthenica? 
#Best transformation
# for distance firstly we calcuated the mean by location and the make the table for calculation

SX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATASRAME
SY=SX%>% 
  filter(Species=="Stipa borysthenica")
SZ = SY %>%
  group_by(Location) %>%
  summarize(
    avg_shoot = mean(shoot, na.rm = TRUE),
    avg_root = mean(root, na.rm = TRUE),
    avg_LDMC = mean(as.numeric(LDMC), na.rm = TRUE),
    avg_SLA = mean(SLA, na.rm = TRUE),
    avg_Flowers = mean(Flowers, na.rm = TRUE),
    avg_Seeds = mean(Seeds, na.rm = TRUE),
    agg_TSW = mean(TSW, na.rm = TRUE)
  )

write.csv(SZ, "SB_Adult_meanshoot_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Stipa borysthenica? 
#Open file
SD<-read_xlsx("SB_Distance_shoot.xlsx")
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
plot(SD$DISTANCE_KM,sqtransSD, main="Connection between distance among locations on C.borysthenica Adult weight",
     ylab="Adult weight difference betweeen locations (SRT) (g)", xlab="Distance (km)")
abline(lm(sqtransSD~SD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransSD.lm = lm(sqtransSD~SD$DISTANCE_KM, data=SD)
summary(sqtransSD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Stipa borysthenica? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)

modelSZ<- lme(shoot~STZ,random = ~ 1 | Location,data=SB)
modelSZ
Anova(modelSZ, type="II")
xSZ = (residuals(modelSZ))
xSZ
#Square root transformation
sqtransSZ = sqrt(SB$shoot)
modelSZ2<-lme(sqtransSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSZ2, type="II")
xSZ2 = (residuals(modelSZ2))
xSZ2
#Log transformation
logtransSZ = log(SB$shoot)
modelSZ3<-lme(logtransSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSZ3, type="II")  ### THIS IS THE ONE
xSZ3 = (residuals(modelSZ3)) 
xSZ3
#Cube root transformation
T_cubSZ = sign(SB$shoot) * abs(SB$shoot)^(1/3)
modelSZ4<-lme(T_cubSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSZ4, type="II")
xSZ4 = (residuals(modelSZ4))
xSZ4
#Squaring transformation
squaringSZ = (SB$shoot)^2
modelSB5<-lm(squaringSZ~STZ,random = ~ 1 | Location, data=SB)
#CORRECTED MODEL SB5
Anova(modelSB5, type="II") 
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


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Stipa borysthenica? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
modelSR <- lme(shoot~Aridity, random = ~ 1 | Location, data=SB)
summary(modelSR)
Anova(modelSR, type="II")
xSR = (residuals(modelSR))
xSR
#Square root transformation
sqtransSR = sqrt(SB$shoot)
modelSR2<-lme(sqtransSR~Aridity,random = ~ 1 | Location, data=SB)
Anova(modelSR2, type="II")
xSR2 = (residuals(modelSR2))
xSR2
#Log transformation
logtransSR = log(SB$shoot)
modelSR3<-lme(logtransSR~Aridity, random = ~ 1 | Location, data=SB)
Anova(modelSR3, type="II") ### THIS IS THE ONE
xSR3 = (residuals(modelSR3))
xSR3
#Cube root transformation
T_cubSR = sign(SB$shoot) * abs(SB$shoot)^(1/3)
modelSR4<-lme(T_cubSR~Aridity,random = ~ 1 | Location, data=SB)
Anova(modelSR4, type="II")  
xSR4 = (residuals(modelSR4))
xSR4
#Squaring transformation
squaringSR = (SB$shoot)^2
modelSR5<-lme(squaringSR~Aridity, random = ~ 1 | Location, data=SB)
Anova(modelSR5, type="II")
xSR5 = (residuals(modelSR5))
xSR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xSR)   #NT
shapiro.test(xSR2)  #Sqrt
shapiro.test(xSR3)  #Log
shapiro.test(xSR4)  #Cubert
shapiro.test(xSR5)  #Squaring
bartlett.test(SB$weight, SB$aridity)  #NT
bartlett.test(sqtransSR, SB$aridity)  #Sqrt
bartlett.test(logtransSR, SB$aridity) #Log
bartlett.test(T_cubSR, SB$aridity)    #Cubert
bartlett.test(squaringSR, SB$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xSR3)
skewness(xSR3)
kurtosis(xSR3)
qqnorm(residuals(modelSR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSR3),
       col="red")
plot(fitted(modelSR3),
     residuals(modelSR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(SB, aes(x = Aridity, y = squaringSR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelSR5)))) 
#

##################################################################
################### ADULT SIZE ANALYSIS (R O O T)
##################
##################################################################
data<-read_xlsx("Adult_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
##Adult maximum leaf height
###Location ~ root
#No transformation
modelSB<- lm(root~Location,data=SB)
modelSB
Anova(modelSB, type="II")
xSB = (residuals(modelSB))
xSB
#Square root transformation
sqtransSB = sqrt(SB$root)
modelSB2<-lm(sqtransSB~Location, data=SB)
Anova(modelSB2, type="II") 
xSB2 = (residuals(modelSB2))
xSB2
#Log transformation
logtransSB = log(SB$root)
modelSB3<-lm(logtransSB~Location, data=SB) 
Anova(modelSB3, type="II")  ####### THIS IS THE ONE
xSB3 = (residuals(modelSB3))
xSB3

#Cube root transformation
T_cubSB = sign(SB$root) * abs(SB$root)^(1/3)
modelSB4<-lm(T_cubSB~Location, data=SB)
Anova(modelSB4, type="II") 
xSB4 = (residuals(modelSB4))
xSB4
#Squaring transformation
squaringSB = (SB$root)^2
modelSB5<-lm(squaringSB~Location, data=SB)
#CORRECTED MODEL SB5
Anova(modelSB5, type="II") 
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
TUKEYSB<- summary(glht(modelSB3, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
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

#What is the effect of Ecological distance (STZ) on Adult weight in Stipa borysthenica? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

SX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATASRAME
SY=SX%>% 
  filter(Species=="Stipa borysthenica")
SZ=SY%>% 
  group_by(Location)%>% 
  summarize(
    avg_root=mean(root),
    avg_root=mean(root))

#write.csv(SZ, "meanroot_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Stipa borysthenica? 
#Open file
SD<-read_xlsx("SB_Distance_root.xlsx")
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
Anova(modelSD2, type="II")  ### THIS iS THE ONE 
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
plot(SD$DISTANCE_KM,sqtransSD, main="Connection between distance among locations on C.borysthenica Adult weight",
     ylab="Adult weight difference betweeen locations (SRT) (g)", xlab="Distance (km)")
abline(lm(sqtransSD~SD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransSD.lm = lm(sqtransSD~SD$DISTANCE_KM, data=SD)
summary(sqtransSD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Stipa borysthenica? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
SB$STZ=as.factor(SB$STZ)
modelSZ<- lme(root~STZ,random = ~ 1 | Location,data=SB)
modelSZ
Anova(modelSZ, type="II")
xSZ = (residuals(modelSZ))
xSZ
#Square root transformation
sqtransSZ = sqrt(SB$root)
modelSZ2<-lme(sqtransSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSZ2, type="II") 
xSZ2 = (residuals(modelSZ2))
xSZ2
#Log transformation
logtransSZ = log(SB$root)
modelSZ3<-lme(logtransSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSZ3, type="II") ### THIS IS THE ONE
xSZ3 = (residuals(modelSZ3))
xSZ3
#Cube root transformation
T_cubSZ = sign(SB$root) * abs(SB$root)^(1/3)
modelSZ4<-lme(T_cubSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSZ4, type="II") 
xSZ4 = (residuals(modelSZ4))
xSZ4
#Squaring transformation
squaringSZ = (SB$root)^2
modelSB5<-lm(squaringSZ~STZ,random = ~ 1 | Location, data=SB)
Anova(modelSB5, type="II") 
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
#What is the effect of Environmental distance (Aridity) on Adult weight in Stipa borysthenica? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
modelSR <- lme(root~Aridity, random = ~ 1 | Location, data=SB)
summary(modelSR)
Anova(modelSR, type="II")
xSR = (residuals(modelSR))
xSR
#Square root transformation
sqtransSR = sqrt(SB$root)
modelSR2<-lme(sqtransSR~Aridity,random = ~ 1 | Location, data=SB)
Anova(modelSR2, type="II") 
xSR2 = (residuals(modelSR2))
xSR2
#Log transformation
logtransSR = log(SB$root)
modelSR3<-lme(logtransSR~Aridity, random = ~ 1 | Location, data=SB)
Anova(modelSR3, type="II") ### THIS IS THE ONE
xSR3 = (residuals(modelSR3))
xSR3
#Cube root transformation
T_cubSR = sign(SB$root) * abs(SB$root)^(1/3)
modelSR4<-lme(T_cubSR~Aridity,random = ~ 1 | Location, data=SB)
Anova(modelSR4, type="II") 
xSR4 = (residuals(modelSR4))
xSR4
#Squaring transformation
squaringSR = (SB$root)^2
modelSR5<-lme(squaringSR~Aridity, random = ~ 1 | Location, data=SB)
Anova(modelSR5, type="II") 
xSR5 = (residuals(modelSR5))
xSR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xSR)   #NT
shapiro.test(xSR2)  #Sqrt
shapiro.test(xSR3)  #Log
shapiro.test(xSR4)  #Cubert
shapiro.test(xSR5)  #Squaring
bartlett.test(SB$weight, SB$aridity)  #NT
bartlett.test(sqtransSR, SB$aridity)  #Sqrt
bartlett.test(logtransSR, SB$aridity) #Log
bartlett.test(T_cubSR, SB$aridity)    #Cubert
bartlett.test(squaringSR, SB$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xSR)
plotNormalHistogram(xSR2)
plotNormalHistogram(xSR3)
plotNormalHistogram(xSR4)
plotNormalHistogram(xSR5)
skewness(xSR3)
kurtosis(xSR3)
qqnorm(residuals(modelSR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSR3),
       col="red")
plot(fitted(modelSR3),
     residuals(modelSR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(SB, aes(x = Aridity, y = squaringSR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelSR5)))) 
#
######################################################
##########  LEAF   SRY  MATTER CONTENT ( L D M C)
###########
#####################################################

data<-read_xlsx("Adult_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
##Adult mass
###Location ~ LDMC
#No transformation
modelSB<- lm(LDMC~Location,data=SB)
modelSB
Anova(modelSB, type="II") ##### THIS IS THE ONE
xSB = (residuals(modelSB))
xSB
class(SB$LDMC)
SB$LDMC=as.numeric(SB$LDMC)
#Square root transformation
sqtransSB = sqrt(SB$LDMC)
modelSB2<-lm(sqtransSB~Location, data=SB)
Anova(modelSB2, type="II")
xSB2 = (residuals(modelSB2))
xSB2
#Log transformation
logtransSB = log(SB$LDMC)
modelSB3<-lm(logtransSB~Location, data=SB)
Anova(modelSB3, type="II") 
xSB3 = (residuals(modelSB3))
xSB3
#Cube root transformation
T_cubSB = sign(SB$LDMC) * abs(SB$LDMC)^(1/3)
modelSB4<-lm(T_cubSB~Location, data=SB)
Anova(modelSB4, type="II")
xSB4 = (residuals(modelSB4))
xSB4
#Squaring transformation
squaringSB = (SB$LDMC)^2
modelSB5<-lm(squaringSB~Location, data=SB)
###CORRECTED MODEL SB5
Anova(modelSB5, type="II") 
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
TUKEYSB<- summary(glht(modelSB, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
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

#What is the effect of Euclidean distance on Adult weight in Stipa borysthenica? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

SX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATASRAME
SY=SX%>% 
  filter(Species=="Stipa borysthenica")
SZ = SY %>%
  group_by(Location) %>%
  summarize(
    avg_LDMC = mean(LDMC, na.rm = TRUE),
    avg_root = mean(root, na.rm = TRUE),
    avg_LDMC = mean(as.numeric(LDMC), na.rm = TRUE),
    avg_SLA = mean(SLA, na.rm = TRUE),
    avg_Flowers = mean(Flowers, na.rm = TRUE),
    avg_Seeds = mean(Seeds, na.rm = TRUE),
    agg_TSW = mean(TSW, na.rm = TRUE)
  )

write.csv(SZ, "SB_Adult_meanLDMC_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Stipa borysthenica? 
#Open file
SD<-read_xlsx("SB_Distance_LDMC.xlsx")
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
plot(SD$DISTANCE_KM,T_cubSD , main="Connection between distance among locations on C.borysthenica Adult weight",
     ylab="Adult weight difference betweeen locations (SRT) (g)", xlab="Distance (km)")
abline(lm(T_cubSD ~SD$DISTANCE_KM), col="red") # regression line (y~x)
T_cubSD.lm = lm(T_cubSD ~SD$DISTANCE_KM, data=SD)
summary(T_cubSD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Stipa borysthenica? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$STZ=as.factor(SB$STZ)
SB$LDMC=as.numeric(SB$LDMC)
modelSZ <- lme(LDMC ~ STZ, random = ~ 1 | Location, data = SB, na.action=na.omit)
modelSZ
Anova(modelSZ, type="II") ### THIS IS THE ONE
xSZ = (residuals(modelSZ))
xSZ
#Square root transformation
sqtransSZ = sqrt(SB$LDMC)
modelSZ2<-lme(sqtransSZ~STZ,random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSZ2, type="II") 
xSZ2 = (residuals(modelSZ2))
xSZ2
#Log transformation
logtransSZ = log(SB$LDMC)
modelSZ3<-lme(logtransSZ~STZ,random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSZ3, type="II") 
xSZ3 = (residuals(modelSZ3)) 
xSZ3
#Cube root transformation
T_cubSZ = sign(SB$LDMC) * abs(SB$LDMC)^(1/3)
modelSZ4<-lme(T_cubSZ~STZ,random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSZ4, type="II")
xSZ4 = (residuals(modelSZ4))
xSZ4
#Squaring transformation
squaringSZ = (SB$LDMC)^2
modelSB5<-lm(squaringSZ~STZ,random = ~ 1 | Location, data=SB, na.action=na.omit)
##CORRECTED MODEL SB5
Anova(modelSB5, type="II") 
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
TUKEYSZ<- summary(glht(modelSZ, linfct = mcp(STZ = "Tukey")), test = adjusted("holm"))
TUKEYSZ
# Assuming TUKEYSB is your summary object
significant_pairs <- sum(TUKEYSZ$test$pvalues < 0.05)
total_pairs <- length(TUKEYSZ$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Stipa borysthenica? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
SB$LDMC=as.numeric(SB$LDMC)
SB$Aridity=as.numeric(SB$Aridity)
modelSR <- lme(LDMC~Aridity, random = ~ 1 | Location, data=SB, na.action=na.omit)
summary(modelSR)
Anova(modelSR, type="II")
xSR = (residuals(modelSR))
xSR
#Square root transformation
sqtransSR = sqrt(SB$LDMC)
modelSR2<-lme(sqtransSR~Aridity,random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSR2, type="II") 
xSR2 = (residuals(modelSR2))
xSR2
#Log transformation
logtransSR = log(SB$LDMC)
modelSR3<-lme(logtransSR~Aridity, random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSR3, type="II")  
xSR3 = (residuals(modelSR3))
xSR3
#Cube root transformation
T_cubSR = sign(SB$LDMC) * abs(SB$LDMC)^(1/3)
modelSR4<-lme(T_cubSR~Aridity,random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSR4, type="II")
xSR4 = (residuals(modelSR4))
xSR4
#Squaring transformation
squaringSR = (SB$LDMC)^2
modelSR5<-lme(squaringSR~Aridity, random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSR5, type="II") ### THIS IS THE ONE
xSR5 = (residuals(modelSR5))
xSR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xSR)   #NT
shapiro.test(xSR2)  #Sqrt
shapiro.test(xSR3)  #Log
shapiro.test(xSR4)  #Cubert
shapiro.test(xSR5)  #Squaring
bartlett.test(SB$weight, SB$aridity)  #NT
bartlett.test(sqtransSR, SB$aridity)  #Sqrt
bartlett.test(logtransSR, SB$aridity) #Log
bartlett.test(T_cubSR, SB$aridity)    #Cubert
bartlett.test(squaringSR, SB$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xSR)
plotNormalHistogram(xSR2)
plotNormalHistogram(xSR3)
plotNormalHistogram(xSR4)
plotNormalHistogram(xSR5)
skewness(xSR3)
kurtosis(xSR3)
qqnorm(residuals(modelSR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSR3),
       col="red")
plot(fitted(modelSR3),
     residuals(modelSR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(SB, aes(x = Aridity, y = squaringSR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelSR5)))) 
#

######################################################
########## SPECIFIC LEAF AREA (S  L  A)
###########
#####################################################
data<-read_xlsx("Adult_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
##Adult mass
###Location ~ SLA
#No transformation
modelSB<- lm(SLA~Location,data=SB)
modelSB
Anova(modelSB, type="II") 
xSB = (residuals(modelSB))
xSB
class(SB$SLA)
SB$SLA=as.numeric(SB$SLA)
#Square root transformation
sqtransSB = sqrt(SB$SLA)
modelSB2<-lm(sqtransSB~Location, data=SB)
Anova(modelSB2, type="II")
xSB2 = (residuals(modelSB2))
xSB2
#Log transformation
logtransSB = log(SB$SLA)
modelSB3<-lm(logtransSB~Location, data=SB)
Anova(modelSB3, type="II") 
xSB3 = (residuals(modelSB3))
xSB3
#Cube root transformation
T_cubSB = sign(SB$SLA) * abs(SB$SLA)^(1/3)
modelSB4<-lm(T_cubSB~Location, data=SB)
Anova(modelSB4, type="II") 
xSB4 = (residuals(modelSB4))
xSB4
#Squaring transformation
squaringSB = (SB$SLA)^2
modelSB5<-lm(squaringSB~Location, data=SB)
####MODEL SB5
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
TUKEYSB<- summary(glht(modelSB, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
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

#What is the effect of Euclidean distance on Adult weight in Stipa borysthenica? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

SX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATASRAME
SY=SX%>% 
  filter(Species=="Stipa borysthenica")
SZ = SY %>%
  group_by(Location) %>%
  summarize(
    avg_SLA = mean(SLA, na.rm = TRUE),
    avg_root = mean(root, na.rm = TRUE),
    avg_SLA = mean(as.numeric(SLA), na.rm = TRUE),
    avg_SLA = mean(SLA, na.rm = TRUE),
    avg_Flowers = mean(Flowers, na.rm = TRUE),
    avg_Seeds = mean(Seeds, na.rm = TRUE),
    agg_TSW = mean(TSW, na.rm = TRUE)
  )

#write.csv(SZ, "SB_Adult_meanSLA_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Stipa borysthenica? 
#Open file
SD<-read_xlsx("SB_Distance_SLA.xlsx")
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
plotNormalHistogram(xSD3)
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
plot(SD$DISTANCE_KM,T_cubSD, main="Connection between distance among locations on C.borysthenica Adult weight",
     ylab="Adult weight difference betweeen locations (SRT) (g)", xlab="Distance (km)")
abline(lm(T_cubSD~SD$DISTANCE_KM), col="red") # regression line (y~x)
T_cubSD.lm = lm(T_cubSD~SD$DISTANCE_KM, data=SD)
summary(T_cubSD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Stipa borysthenica? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
SB$SLA=as.numeric(SB$SLA)
modelSZ <- lme(SLA ~ STZ, random = ~ 1 | Location, data = SB, na.action=na.omit)
modelSZ
Anova(modelSZ, type="II")
xSZ = (residuals(modelSZ))
xSZ
#Square root transformation
sqtransSZ = sqrt(SB$SLA)
modelSZ2<-lme(sqtransSZ~STZ,random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSZ2, type="II") 
xSZ2 = (residuals(modelSZ2))
xSZ2
#Log transformation
logtransSZ = log(SB$SLA)
modelSZ3<-lme(logtransSZ~STZ,random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSZ3, type="II") 
xSZ3 = (residuals(modelSZ3)) 
xSZ3
#Cube root transformation
T_cubSZ = sign(SB$SLA) * abs(SB$SLA)^(1/3)
modelSZ4<-lme(T_cubSZ~STZ,random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSZ4, type="II")
xSZ4 = (residuals(modelSZ4))
xSZ4
#Squaring transformation
squaringSZ = (SB$SLA)^2
modelSB5<-lm(squaringSZ~STZ,random = ~ 1 | Location, data=SB, na.action=na.omit)
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
#What is the effect of Environmental distance (Aridity) on Adult weight in Stipa borysthenica? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
SB=data%>% 
  filter(Species=="Stipa borysthenica")
SB$Location=as.factor(SB$Location)
SB$SLA=as.numeric(SB$SLA)
SB$Aridity=as.numeric(SB$Aridity)
modelSR <- lme(SLA~Aridity, random = ~ 1 | Location, data=SB, na.action=na.omit)
summary(modelSR)
Anova(modelSR, type="II")
xSR = (residuals(modelSR))
xSR
#Square root transformation
sqtransSR = sqrt(SB$SLA)
modelSR2<-lme(sqtransSR~Aridity,random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSR2, type="II") 
xSR2 = (residuals(modelSR2))
xSR2
#Log transformation
logtransSR = log(SB$SLA)
modelSR3<-lme(logtransSR~Aridity, random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSR3, type="II")  
xSR3 = (residuals(modelSR3))
xSR3
#Cube root transformation
T_cubSR = sign(SB$SLA) * abs(SB$SLA)^(1/3)
modelSR4<-lme(T_cubSR~Aridity,random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSR4, type="II")
xSR4 = (residuals(modelSR4))
xSR4
#Squaring transformation
squaringSR = (SB$SLA)^2
modelSR5<-lme(squaringSR~Aridity, random = ~ 1 | Location, data=SB, na.action=na.omit)
Anova(modelSR5, type="II") ## THIS IS THE ONE
xSR5 = (residuals(modelSR5))
xSR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xSR)   #NT
shapiro.test(xSR2)  #Sqrt
shapiro.test(xSR3)  #Log
shapiro.test(xSR4)  #Cubert
shapiro.test(xSR5)  #Squaring
bartlett.test(SB$weight, SB$aridity)  #NT
bartlett.test(sqtransSR, SB$aridity)  #Sqrt
bartlett.test(logtransSR, SB$aridity) #Log
bartlett.test(T_cubSR, SB$aridity)    #Cubert
bartlett.test(squaringSR, SB$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xSR)
plotNormalHistogram(xSR2)
plotNormalHistogram(xSR3)
plotNormalHistogram(xSR4)
plotNormalHistogram(xSR5)
skewness(xSR3)
kurtosis(xSR3)
qqnorm(residuals(modelSR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelSR3),
       col="red")
plot(fitted(modelSR3),
     residuals(modelSR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(SB, aes(x = Aridity, y = squaringSR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelSR5)))) 
#











