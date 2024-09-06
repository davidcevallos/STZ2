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
FV=data%>% 
  filter(Species=="Festuca vaginata")
FV$Location=as.factor(FV$Location)
##Adult mass
###Location ~ shoot
#No transformation
modelFV<- lm(shoot~Location,data=FV)
modelFV
Anova(modelFV, type="II")
xFV = (residuals(modelFV))
xFV
#Square root transformation
sqtransFV = sqrt(FV$shoot)
modelFV2<-lm(sqtransFV~Location, data=FV)
Anova(modelFV2, type="II")
xFV2 = (residuals(modelFV2))
xFV2
#Log transformation
logtransFV = log(FV$shoot)
modelFV3<-lm(logtransFV~Location, data=FV)
Anova(modelFV3, type="II") 
xFV3 = (residuals(modelFV3))
xFV3

#Cube root transformation
T_cubFV = sign(FV$shoot) * abs(FV$shoot)^(1/3)
modelFV4<-lm(T_cubFV~Location, data=FV)
Anova(modelFV4, type="II") ##### THIS IS THE ONE
xFV4 = (residuals(modelFV4))
xFV4
#Squaring transformation
squaringFV = (FV$shoot)^2
modelFV5<-lm(squaringFV~Location, data=FV)
#CORRECTED FV5
Anova(modelFV5, type="II") 
xFV5 = (residuals(modelFV5))
xFV5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFV)   #NT
shapiro.test(xFV2)  #Sqrt
shapiro.test(xFV3)  #Log
shapiro.test(xFV4)  #Cubert
shapiro.test(xFV5)  #Squaring

leveneTest(xFV~Location, FV) #NT
leveneTest(xFV2~Location, FV) #Sqrt
leveneTest(xFV3~Location, FV) #Log
leveneTest(xFV4~Location, FV) #Cubert
leveneTest(xFV5~Location, FV) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xFV)
plotNormalHistogram(xFV2)
plotNormalHistogram(xFV3)
plotNormalHistogram(xFV4)
plotNormalHistogram(xFV5)

qqnorm(residuals(modelFV),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV),
       col="red")

qqnorm(residuals(modelFV2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV2),
       col="red")

qqnorm(residuals(modelFV3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV3),
       col="red")

qqnorm(residuals(modelFV4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV4),
       col="red")

qqnorm(residuals(modelFV5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV5),
       col="red")

plot(fitted(modelFV),
     residuals(modelFV))

plot(fitted(modelFV2),
     residuals(modelFV2))

plot(fitted(modelFV3),
     residuals(modelFV3))

plot(fitted(modelFV4),
     residuals(modelFV4))


plot(fitted(modelFV5),
     residuals(modelFV5))
TUKEYFV<- summary(glht(modelFV4, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYFV
# Assuming TUKEYFV is your summary object
significant_pairs <- sum(TUKEYFV$test$pvalues < 0.05)
total_pairs <- length(TUKEYFV$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")


tFV<- glht(modelFV3, linfct = mcp(location= "Tukey"))
t.cldFV <- cld(tFV)   # letter-based display
t.cldFV

#What is the effect of Euclidean distance on Adult weight in Festuca vaginata? 
#Best transformation
# for distance firstly we calcuated the mean by location and the make the table for calculation

FX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATAFRAME
FY=FX%>% 
  filter(Species=="Festuca vaginata")
FZ = FY %>%
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

write.csv(FZ, "FV_Adult_meanshoot_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Festuca vaginata? 
#Open file
FD<-read_xlsx("FV_Distance_shoot.xlsx")
FD
#Best transformation
#No transformation
modelFD <- lm(Absolute~DISTANCE_KM , data=FD)
summary(modelFD)
Anova(modelFD, type="II")
xFD = (residuals(modelFD))
xFD
#Square root transformation
sqtransFD = sqrt(FD$Absolute)
modelFD2<-lm(sqtransFD~DISTANCE_KM, data=FD)
Anova(modelFD2, type="II") ### THIS iS THE ONE
xFD2 = (residuals(modelFD2))
xFD2
#Log transformation
logtransFD = log(FD$Absolute)
modelFD3<-lm(logtransFD~DISTANCE_KM,  data=FD)
Anova(modelFD3, type="II") 
xFD3 = (residuals(modelFD3))
xFD3
#Cube root transformation
T_cubFD = sign(FD$Absolute) * abs(FD$Absolute)^(1/3)
modelFD4<-lm(T_cubFD~DISTANCE_KM,data=FD)
Anova(modelFD4, type="II")     
xFD4 = (residuals(modelFD4))
xFD4
#Squaring transformation
squaringFD = (FD$Absolute)^2
modelFD5<-lm(squaringFD~DISTANCE_KM,  data=FD)
Anova(modelFD5, type="II")
xFD5 = (residuals(modelFD5))
xFD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFD)   #NT
shapiro.test(xFD2)  #Sqrt
shapiro.test(xFD3)  #Log
shapiro.test(xFD4)  #Cubert
shapiro.test(xFD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xFD)
plotNormalHistogram(xFD2)
plotNormalHistogram(xFD4)
plotNormalHistogram(xFD5)

qqnorm(residuals(modelFD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD),
       col="red")

qqnorm(residuals(modelFD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD2),
       col="red")

qqnorm(residuals(modelFD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD4),
       col="red")

qqnorm(residuals(modelFD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD5),
       col="red")


plot(fitted(modelFD),
     residuals(modelFD))

plot(fitted(modelFD2),
     residuals(modelFD2))

plot(fitted(modelFD4),
     residuals(modelFD4))

plot(fitted(modelFD5),
     residuals(modelFD5))


#Distance- Absolute weight relationship plotting
##regression
plot(FD$DISTANCE_KM,sqtransFD, main="Connection between distance among locations on C.vaginata Adult weight",
     ylab="Adult weight difference betweeen locations (FRT) (g)", xlab="Distance (km)")
abline(lm(sqtransFD~FD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransFD.lm = lm(sqtransFD~FD$DISTANCE_KM, data=FD)
summary(sqtransFD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Festuca vaginata? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
FV=data%>% 
  filter(Species=="Festuca vaginata")
FV$Location=as.factor(FV$Location)

modelFZ<- lme(shoot~STZ,random = ~ 1 | Location,data=FV)
modelFZ
Anova(modelFZ, type="II")
xFZ = (residuals(modelFZ))
xFZ
#Square root transformation
sqtransFZ = sqrt(FV$shoot)
modelFZ2<-lme(sqtransFZ~STZ,random = ~ 1 | Location, data=FV)
Anova(modelFZ2, type="II")
xFZ2 = (residuals(modelFZ2))
xFZ2
#Log transformation
logtransFZ = log(FV$shoot)
modelFZ3<-lme(logtransFZ~STZ,random = ~ 1 | Location, data=FV)
Anova(modelFZ3, type="II") 
xFZ3 = (residuals(modelFZ3)) 
xFZ3
#Cube root transformation
T_cubFZ = sign(FV$shoot) * abs(FV$shoot)^(1/3)
modelFZ4<-lme(T_cubFZ~STZ,random = ~ 1 | Location, data=FV)
Anova(modelFZ4, type="II") ### THIS IS THE ONE
xFZ4 = (residuals(modelFZ4))
xFZ4
#Squaring transformation
squaringFZ = (FV$shoot)^2
modelFV5<-lm(squaringFZ~STZ,random = ~ 1 | Location, data=FV)
#MODELFV5
Anova(modelFV5, type="II") 
xFZ5 = (residuals(modelFV5))
xFZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFZ)   #NT
shapiro.test(xFZ2)  #Sqrt
shapiro.test(xFZ3)  #Log
shapiro.test(xFZ4)  #Cubert
shapiro.test(xFZ5)  #Squaring
leveneTest(xFZ~STZ, FV) #NT
leveneTest(xFZ2~STZ, FV) #Sqrt
leveneTest(xFZ3~STZ, FV) #Log
leveneTest(xFZ4~STZ, FV) #Cubert
leveneTest(xFZ5~STZ, FV) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xFZ3)
skewness(xFZ3)
kurtosis(xFZ3)
qqnorm(residuals(modelFZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFZ3),
       col="red")
plot(fitted(modelFZ3),
     residuals(modelFZ))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Festuca vaginata? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
FV=data%>% 
  filter(Species=="Festuca vaginata")
FV$Location=as.factor(FV$Location)
modelFR <- lme(shoot~Aridity, random = ~ 1 | Location, data=FV)
summary(modelFR)
Anova(modelFR, type="II")
xFR = (residuals(modelFR))
xFR
#Square root transformation
sqtransFR = sqrt(FV$shoot)
modelFR2<-lme(sqtransFR~Aridity,random = ~ 1 | Location, data=FV)
Anova(modelFR2, type="II")
xFR2 = (residuals(modelFR2))
xFR2
#Log transformation
logtransFR = log(FV$shoot)
modelFR3<-lme(logtransFR~Aridity, random = ~ 1 | Location, data=FV)
Anova(modelFR3, type="II") 
xFR3 = (residuals(modelFR3))
xFR3
#Cube root transformation
T_cubFR = sign(FV$shoot) * abs(FV$shoot)^(1/3)
modelFR4<-lme(T_cubFR~Aridity,random = ~ 1 | Location, data=FV)
Anova(modelFR4, type="II")  ### THIS IS THE ONE
xFR4 = (residuals(modelFR4))
xFR4
#Squaring transformation
squaringFR = (FV$shoot)^2
modelFR5<-lme(squaringFR~Aridity, random = ~ 1 | Location, data=FV)
Anova(modelFR5, type="II")
xFR5 = (residuals(modelFR5))
xFR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFR)   #NT
shapiro.test(xFR2)  #Sqrt
shapiro.test(xFR3)  #Log
shapiro.test(xFR4)  #Cubert
shapiro.test(xFR5)  #Squaring
bartlett.test(FV$weight, FV$aridity)  #NT
bartlett.test(sqtransFR, FV$aridity)  #Sqrt
bartlett.test(logtransFR, FV$aridity) #Log
bartlett.test(T_cubFR, FV$aridity)    #Cubert
bartlett.test(squaringFR, FV$aridity) #Squaring

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
ggplot(FV, aes(x = Aridity, y = squaringFR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelFR5)))) 
#

##################################################################
################### ADULT SIZE ANALYSIS (R O O T)
##################
##################################################################

data<-read_xlsx("Adult_Masters.xlsx")
FV=data%>% 
  filter(Species=="Festuca vaginata")
FV$Location=as.factor(FV$Location)
##Adult maximum leaf height
###Location ~ root
#No transformation
modelFV<- lm(root~Location,data=FV)
modelFV
Anova(modelFV, type="II")
xFV = (residuals(modelFV))
xFV
#Square root transformation
sqtransFV = sqrt(FV$root)
modelFV2<-lm(sqtransFV~Location, data=FV)
Anova(modelFV2, type="II") ####### THIS IS THE ONE
xFV2 = (residuals(modelFV2))
xFV2
#Log transformation
logtransFV = log(FV$root)
modelFV3<-lm(logtransFV~Location, data=FV)
Anova(modelFV3, type="II")  
xFV3 = (residuals(modelFV3))
xFV3

#Cube root transformation
T_cubFV = sign(FV$root) * abs(FV$root)^(1/3)
modelFV4<-lm(T_cubFV~Location, data=FV)
Anova(modelFV4, type="II") 
xFV4 = (residuals(modelFV4))
xFV4
#Squaring transformation
squaringFV = (FV$root)^2
modelFV5<-lm(squaringFV~Location, data=FV)
#MODEL FV5
Anova(modelFV5, type="II") 
xFV5 = (residuals(modelFV5))
xFV5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFV)   #NT
shapiro.test(xFV2)  #Sqrt
shapiro.test(xFV3)  #Log
shapiro.test(xFV4)  #Cubert
shapiro.test(xFV5)  #Squaring

leveneTest(xFV~Location, FV) #NT
leveneTest(xFV2~Location, FV) #Sqrt
leveneTest(xFV3~Location, FV) #Log
leveneTest(xFV4~Location, FV) #Cubert
leveneTest(xFV5~Location, FV) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xFV)
plotNormalHistogram(xFV2)
plotNormalHistogram(xFV3)
plotNormalHistogram(xFV4)
plotNormalHistogram(xFV5)

qqnorm(residuals(modelFV),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV),
       col="red")

qqnorm(residuals(modelFV2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV2),
       col="red")

qqnorm(residuals(modelFV3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV3),
       col="red")

qqnorm(residuals(modelFV4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV4),
       col="red")

qqnorm(residuals(modelFV5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV5),
       col="red")

plot(fitted(modelFV),
     residuals(modelFV))

plot(fitted(modelFV2),
     residuals(modelFV2))

plot(fitted(modelFV3),
     residuals(modelFV3))

plot(fitted(modelFV4),
     residuals(modelFV4))


plot(fitted(modelFV5),
     residuals(modelFV5))
TUKEYFV<- summary(glht(modelFV2, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYFV
# Assuming TUKEYFV is your summary object
significant_pairs <- sum(TUKEYFV$test$pvalues < 0.05)
total_pairs <- length(TUKEYFV$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")
tFV<- glht(modelFV3, linfct = mcp(location= "Tukey"))
t.cldFV <- cld(tFV)   # letter-based display
t.cldFV

#What is the effect of Ecological distance (STZ) on Adult weight in Festuca vaginata? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

FX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATAFRAME
FY=FX%>% 
  filter(Species=="Festuca vaginata")
FZ=FY%>% 
  group_by(Location)%>% 
  summarize(
    avg_root=mean(root),
    avg_root=mean(root))

#write.csv(FZ, "meanroot_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Festuca vaginata? 
#Open file
FD<-read_xlsx("FV_Distance_root.xlsx")
FD
#Best transformation
#No transformation
modelFD <- lm(Absolute~DISTANCE_KM , data=FD)
summary(modelFD)
Anova(modelFD, type="II")
xFD = (residuals(modelFD))
xFD
#Square root transformation
sqtransFD = sqrt(FD$Absolute)
modelFD2<-lm(sqtransFD~DISTANCE_KM, data=FD)
Anova(modelFD2, type="II")  ### THIS iS THE ONE 
xFD2 = (residuals(modelFD2))
xFD2
#Log transformation
logtransFD = log(FD$Absolute)
modelFD3<-lm(logtransFD~DISTANCE_KM,  data=FD)
Anova(modelFD3, type="II")
xFD3 = (residuals(modelFD3))
xFD3
#Cube root transformation
T_cubFD = sign(FD$Absolute) * abs(FD$Absolute)^(1/3)
modelFD4<-lm(T_cubFD~DISTANCE_KM,data=FD)
Anova(modelFD4, type="II")  
xFD4 = (residuals(modelFD4))
xFD4
#Squaring transformation
squaringFD = (FD$Absolute)^2
modelFD5<-lm(squaringFD~DISTANCE_KM,  data=FD)
Anova(modelFD5, type="II")
xFD5 = (residuals(modelFD5))
xFD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFD)   #NT
shapiro.test(xFD2)  #Sqrt
shapiro.test(xFD3)  #Log
shapiro.test(xFD4)  #Cubert
shapiro.test(xFD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xFD)
plotNormalHistogram(xFD2)
plotNormalHistogram(xFD4)
plotNormalHistogram(xFD5)

qqnorm(residuals(modelFD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD),
       col="red")

qqnorm(residuals(modelFD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD2),
       col="red")

qqnorm(residuals(modelFD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD4),
       col="red")

qqnorm(residuals(modelFD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD5),
       col="red")


plot(fitted(modelFD),
     residuals(modelFD))

plot(fitted(modelFD2),
     residuals(modelFD2))

plot(fitted(modelFD4),
     residuals(modelFD4))

plot(fitted(modelFD5),
     residuals(modelFD5))


#Distance- Absolute weight relationship plotting
##regression
plot(FD$DISTANCE_KM,sqtransFD, main="Connection between distance among locations on C.vaginata Adult weight",
     ylab="Adult weight difference betweeen locations (FRT) (g)", xlab="Distance (km)")
abline(lm(sqtransFD~FD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransFD.lm = lm(sqtransFD~FD$DISTANCE_KM, data=FD)
summary(sqtransFD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Festuca vaginata? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
FV=data%>% 
  filter(Species=="Festuca vaginata")
FV$Location=as.factor(FV$Location)
FV$STZ=as.factor(FV$STZ)
modelFZ<- lme(root~STZ,random = ~ 1 | Location,data=FV)
modelFZ
Anova(modelFZ, type="II")
xFZ = (residuals(modelFZ))
xFZ
#Square root transformation
sqtransFZ = sqrt(FV$root)
modelFZ2<-lme(sqtransFZ~STZ,random = ~ 1 | Location, data=FV)
Anova(modelFZ2, type="II") ### THIS IS THE ONE
xFZ2 = (residuals(modelFZ2))
xFZ2
#Log transformation
logtransFZ = log(FV$root)
modelFZ3<-lme(logtransFZ~STZ,random = ~ 1 | Location, data=FV)
Anova(modelFZ3, type="II") 
xFZ3 = (residuals(modelFZ3))
xFZ3
#Cube root transformation
T_cubFZ = sign(FV$root) * abs(FV$root)^(1/3)
modelFZ4<-lme(T_cubFZ~STZ,random = ~ 1 | Location, data=FV)
Anova(modelFZ4, type="II") 
xFZ4 = (residuals(modelFZ4))
xFZ4
#Squaring transformation
squaringFZ = (FV$root)^2
modelFV5<-lm(squaringFZ~STZ,random = ~ 1 | Location, data=FV)
Anova(modelFV5, type="II") 
xFZ5 = (residuals(modelFV5))
xFZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFZ)   #NT
shapiro.test(xFZ2)  #Sqrt
shapiro.test(xFZ3)  #Log
shapiro.test(xFZ4)  #Cubert
shapiro.test(xFZ5)  #Squaring
leveneTest(xFZ~STZ, FV) #NT
leveneTest(xFZ2~STZ, FV) #Sqrt
leveneTest(xFZ3~STZ, FV) #Log
leveneTest(xFZ4~STZ, FV) #Cubert
leveneTest(xFZ5~STZ, FV) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xFZ3)
skewness(xFZ3)
kurtosis(xFZ3)
qqnorm(residuals(modelFZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFZ3),
       col="red")
plot(fitted(modelFZ3),
     residuals(modelFZ))
TUKEYFZ<- summary(glht(modelFV5, linfct = mcp(STZ = "Tukey")), test = adjusted("holm"))
TUKEYFZ
# Assuming TUKEYFV is your summary object
significant_pairs <- sum(TUKEYFZ$test$pvalues < 0.05)
total_pairs <- length(TUKEYFZ$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")
#tFV<- glht(modelFV3, linfct = mcp(location= "Tukey"))
#t.cldFV <- cld(tFV)   # letter-based display
#t.cldFV

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Festuca vaginata? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
FV=data%>% 
  filter(Species=="Festuca vaginata")
FV$Location=as.factor(FV$Location)
modelFR <- lme(root~Aridity, random = ~ 1 | Location, data=FV)
summary(modelFR)
Anova(modelFR, type="II")
xFR = (residuals(modelFR))
xFR
#Square root transformation
sqtransFR = sqrt(FV$root)
modelFR2<-lme(sqtransFR~Aridity,random = ~ 1 | Location, data=FV)
Anova(modelFR2, type="II") ### THIS IS THE ONE
xFR2 = (residuals(modelFR2))
xFR2
#Log transformation
logtransFR = log(FV$root)
modelFR3<-lme(logtransFR~Aridity, random = ~ 1 | Location, data=FV)
Anova(modelFR3, type="II") 
xFR3 = (residuals(modelFR3))
xFR3
#Cube root transformation
T_cubFR = sign(FV$root) * abs(FV$root)^(1/3)
modelFR4<-lme(T_cubFR~Aridity,random = ~ 1 | Location, data=FV)
Anova(modelFR4, type="II") 
xFR4 = (residuals(modelFR4))
xFR4
#Squaring transformation
squaringFR = (FV$root)^2
modelFR5<-lme(squaringFR~Aridity, random = ~ 1 | Location, data=FV)
Anova(modelFR5, type="II") 
xFR5 = (residuals(modelFR5))
xFR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFR)   #NT
shapiro.test(xFR2)  #Sqrt
shapiro.test(xFR3)  #Log
shapiro.test(xFR4)  #Cubert
shapiro.test(xFR5)  #Squaring
bartlett.test(FV$weight, FV$aridity)  #NT
bartlett.test(sqtransFR, FV$aridity)  #Sqrt
bartlett.test(logtransFR, FV$aridity) #Log
bartlett.test(T_cubFR, FV$aridity)    #Cubert
bartlett.test(squaringFR, FV$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xFR)
plotNormalHistogram(xFR2)
plotNormalHistogram(xFR3)
plotNormalHistogram(xFR4)
plotNormalHistogram(xFR5)
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
ggplot(FV, aes(x = Aridity, y = squaringFR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelFR5)))) 
#
######################################################
##########  LEAF   FRY  MATTER CONTENT ( L D M C)
###########
#####################################################

data<-read_xlsx("Adult_Masters.xlsx")
FV=data%>% 
  filter(Species=="Festuca vaginata")
FV$Location=as.factor(FV$Location)
##Adult mass
###Location ~ LDMC
#No transformation
modelFV<- lm(LDMC~Location,data=FV)
modelFV
Anova(modelFV, type="II") 
xFV = (residuals(modelFV))
xFV
class(FV$LDMC)
FV$LDMC=as.numeric(FV$LDMC)
#Square root transformation
sqtransFV = sqrt(FV$LDMC)
modelFV2<-lm(sqtransFV~Location, data=FV)
Anova(modelFV2, type="II")
xFV2 = (residuals(modelFV2))
xFV2
#Log transformation
logtransFV = log(FV$LDMC)
modelFV3<-lm(logtransFV~Location, data=FV)
Anova(modelFV3, type="II") 
xFV3 = (residuals(modelFV3))
xFV3
#Cube root transformation
T_cubFV = sign(FV$LDMC) * abs(FV$LDMC)^(1/3)
modelFV4<-lm(T_cubFV~Location, data=FV)
Anova(modelFV4, type="II")
xFV4 = (residuals(modelFV4))
xFV4
#Squaring transformation
squaringFV = (FV$LDMC)^2
modelFV5<-lm(squaringFV~Location, data=FV)
#MODELFV5
Anova(modelFV5, type="II") ##### THIS IS THE ONE
xFV5 = (residuals(modelFV5))
xFV5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFV)   #NT
shapiro.test(xFV2)  #Sqrt
shapiro.test(xFV3)  #Log
shapiro.test(xFV4)  #Cubert
shapiro.test(xFV5)  #Squaring

leveneTest(xFV~Location, FV) #NT
leveneTest(xFV2~Location, FV) #Sqrt
leveneTest(xFV3~Location, FV) #Log
leveneTest(xFV4~Location, FV) #Cubert
leveneTest(xFV5~Location, FV) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xFV)
plotNormalHistogram(xFV2)
plotNormalHistogram(xFV3)
plotNormalHistogram(xFV4)
plotNormalHistogram(xFV5)

qqnorm(residuals(modelFV),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV),
       col="red")

qqnorm(residuals(modelFV2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV2),
       col="red")

qqnorm(residuals(modelFV3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV3),
       col="red")

qqnorm(residuals(modelFV4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV4),
       col="red")

qqnorm(residuals(modelFV5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV5),
       col="red")

plot(fitted(modelFV),
     residuals(modelFV))

plot(fitted(modelFV2),
     residuals(modelFV2))

plot(fitted(modelFV3),
     residuals(modelFV3))

plot(fitted(modelFV4),
     residuals(modelFV4))


plot(fitted(modelFV5),
     residuals(modelFV5))
TUKEYFV<- summary(glht(modelFV5, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYFV
# Assuming TUKEYFV is your summary object
significant_pairs <- sum(TUKEYFV$test$pvalues < 0.05)
total_pairs <- length(TUKEYFV$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")


tFV<- glht(modelFV3, linfct = mcp(location= "Tukey"))
t.cldFV <- cld(tFV)   # letter-based display
t.cldFV

#What is the effect of Euclidean distance on Adult weight in Festuca vaginata? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

FX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATAFRAME
FY=FX%>% 
  filter(Species=="Festuca vaginata")
FZ = FY %>%
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

write.csv(FZ, "FV_Adult_meanLDMC_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Festuca vaginata? 
#Open file
FD<-read_xlsx("FV_Distance_LDMC.xlsx")
FD
#Best transformation
#No transformation
modelFD <- lm(Absolute~DISTANCE_KM , data=FD)
summary(modelFD)
Anova(modelFD, type="II")
xFD = (residuals(modelFD))
xFD
#Square root transformation
sqtransFD = sqrt(FD$Absolute)
modelFD2<-lm(sqtransFD~DISTANCE_KM, data=FD)
Anova(modelFD2, type="II") 
xFD2 = (residuals(modelFD2))
xFD2
#Log transformation
logtransFD = log(FD$Absolute)
modelFD3<-lm(logtransFD~DISTANCE_KM,  data=FD)
Anova(modelFD3, type="II") 
xFD3 = (residuals(modelFD3))
xFD3
#Cube root transformation
T_cubFD = sign(FD$Absolute) * abs(FD$Absolute)^(1/3)
modelFD4<-lm(T_cubFD~DISTANCE_KM,data=FD)
Anova(modelFD4, type="II")   ### THIS iS THE ONE 
xFD4 = (residuals(modelFD4))
xFD4
#Squaring transformation
squaringFD = (FD$Absolute)^2
modelFD5<-lm(squaringFD~DISTANCE_KM,  data=FD)
Anova(modelFD5, type="II")
xFD5 = (residuals(modelFD5))
xFD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFD)   #NT
shapiro.test(xFD2)  #Sqrt
shapiro.test(xFD3)  #Log
shapiro.test(xFD4)  #Cubert
shapiro.test(xFD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xFD)
plotNormalHistogram(xFD2)
plotNormalHistogram(xFD4)
plotNormalHistogram(xFD5)

qqnorm(residuals(modelFD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD),
       col="red")

qqnorm(residuals(modelFD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD2),
       col="red")

qqnorm(residuals(modelFD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD4),
       col="red")

qqnorm(residuals(modelFD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD5),
       col="red")


plot(fitted(modelFD),
     residuals(modelFD))

plot(fitted(modelFD2),
     residuals(modelFD2))

plot(fitted(modelFD4),
     residuals(modelFD4))

plot(fitted(modelFD5),
     residuals(modelFD5))


#Distance- Absolute weight relationship plotting
##regression
plot(FD$DISTANCE_KM,T_cubFD , main="Connection between distance among locations on C.vaginata Adult weight",
     ylab="Adult weight difference betweeen locations (FRT) (g)", xlab="Distance (km)")
abline(lm(T_cubFD ~FD$DISTANCE_KM), col="red") # regression line (y~x)
T_cubFD.lm = lm(T_cubFD ~FD$DISTANCE_KM, data=FD)
summary(T_cubFD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Festuca vaginata? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
FV=data%>% 
  filter(Species=="Festuca vaginata")
FV$Location=as.factor(FV$Location)
FV$LDMC=as.numeric(FV$LDMC)
modelFZ <- lme(LDMC ~ STZ, random = ~ 1 | Location, data = FV, na.action=na.omit)
modelFZ
Anova(modelFZ, type="II") ### THIS IS THE ONE
xFZ = (residuals(modelFZ))
xFZ
#Square root transformation
sqtransFZ = sqrt(FV$LDMC)
modelFZ2<-lme(sqtransFZ~STZ,random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFZ2, type="II") 
xFZ2 = (residuals(modelFZ2))
xFZ2
#Log transformation
logtransFZ = log(FV$LDMC)
modelFZ3<-lme(logtransFZ~STZ,random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFZ3, type="II") 
xFZ3 = (residuals(modelFZ3)) 
xFZ3
#Cube root transformation
T_cubFZ = sign(FV$LDMC) * abs(FV$LDMC)^(1/3)
modelFZ4<-lme(T_cubFZ~STZ,random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFZ4, type="II")
xFZ4 = (residuals(modelFZ4))
xFZ4
#Squaring transformation
squaringFZ = (FV$LDMC)^2
modelFV5<-lm(squaringFZ~STZ,random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFV5, type="II") 
xFZ5 = (residuals(modelFV5))
xFZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFZ)   #NT
shapiro.test(xFZ2)  #Sqrt
shapiro.test(xFZ3)  #Log
shapiro.test(xFZ4)  #Cubert
shapiro.test(xFZ5)  #Squaring
leveneTest(xFZ~STZ, FV) #NT
leveneTest(xFZ2~STZ, FV) #Sqrt
leveneTest(xFZ3~STZ, FV) #Log
leveneTest(xFZ4~STZ, FV) #Cubert
leveneTest(xFZ5~STZ, FV) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xFZ3)
skewness(xFZ3)
kurtosis(xFZ3)
qqnorm(residuals(modelFZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFZ3),
       col="red")
plot(fitted(modelFZ3),
     residuals(modelFZ))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Festuca vaginata? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
FV=data%>% 
  filter(Species=="Festuca vaginata")
FV$Location=as.factor(FV$Location)
FV$LDMC=as.numeric(FV$LDMC)
FV$Aridity=as.numeric(FV$Aridity)
modelFR <- lme(LDMC~Aridity, random = ~ 1 | Location, data=FV, na.action=na.omit)
summary(modelFR)
Anova(modelFR, type="II")
xFR = (residuals(modelFR))
xFR
#Square root transformation
sqtransFR = sqrt(FV$LDMC)
modelFR2<-lme(sqtransFR~Aridity,random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFR2, type="II") 
xFR2 = (residuals(modelFR2))
xFR2
#Log transformation
logtransFR = log(FV$LDMC)
modelFR3<-lme(logtransFR~Aridity, random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFR3, type="II")  
xFR3 = (residuals(modelFR3))
xFR3
#Cube root transformation
T_cubFR = sign(FV$LDMC) * abs(FV$LDMC)^(1/3)
modelFR4<-lme(T_cubFR~Aridity,random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFR4, type="II")
xFR4 = (residuals(modelFR4))
xFR4
#Squaring transformation
squaringFR = (FV$LDMC)^2
modelFR5<-lme(squaringFR~Aridity, random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFR5, type="II") ### THIS IS THE ONE
xFR5 = (residuals(modelFR5))
xFR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFR)   #NT
shapiro.test(xFR2)  #Sqrt
shapiro.test(xFR3)  #Log
shapiro.test(xFR4)  #Cubert
shapiro.test(xFR5)  #Squaring
bartlett.test(FV$weight, FV$aridity)  #NT
bartlett.test(sqtransFR, FV$aridity)  #Sqrt
bartlett.test(logtransFR, FV$aridity) #Log
bartlett.test(T_cubFR, FV$aridity)    #Cubert
bartlett.test(squaringFR, FV$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xFR)
plotNormalHistogram(xFR2)
plotNormalHistogram(xFR3)
plotNormalHistogram(xFR4)
plotNormalHistogram(xFR5)
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
ggplot(FV, aes(x = Aridity, y = squaringFR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelFR5)))) 
#

######################################################
########## SPECIFIC LEAF AREA (S  L  A)
###########
#####################################################
data<-read_xlsx("Adult_Masters.xlsx")
FV=data%>% 
  filter(Species=="Festuca vaginata")
FV$Location=as.factor(FV$Location)
##Adult mass
###Location ~ SLA
#No transformation
modelFV<- lm(SLA~Location,data=FV)
modelFV
Anova(modelFV, type="II") ##### THIS IS THE ONE
xFV = (residuals(modelFV))
xFV
class(FV$SLA)
FV$SLA=as.numeric(FV$SLA)
#Square root transformation
sqtransFV = sqrt(FV$SLA)
modelFV2<-lm(sqtransFV~Location, data=FV)
Anova(modelFV2, type="II")
xFV2 = (residuals(modelFV2))
xFV2
#Log transformation
logtransFV = log(FV$SLA)
modelFV3<-lm(logtransFV~Location, data=FV)
Anova(modelFV3, type="II") 
xFV3 = (residuals(modelFV3))
xFV3
#Cube root transformation
T_cubFV = sign(FV$SLA) * abs(FV$SLA)^(1/3)
modelFV4<-lm(T_cubFV~Location, data=FV)
Anova(modelFV4, type="II") 
xFV4 = (residuals(modelFV4))
xFV4
#Squaring transformation
squaringFV = (FV$SLA)^2
modelFV5<-lm(squaringFV~Location, data=FV)
#MODELFV5           
Anova(modelFV5, type="II") 
xFV5 = (residuals(modelFV5))
xFV5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFV)   #NT
shapiro.test(xFV2)  #Sqrt
shapiro.test(xFV3)  #Log
shapiro.test(xFV4)  #Cubert
shapiro.test(xFV5)  #Squaring

leveneTest(xFV~Location, FV) #NT
leveneTest(xFV2~Location, FV) #Sqrt
leveneTest(xFV3~Location, FV) #Log
leveneTest(xFV4~Location, FV) #Cubert
leveneTest(xFV5~Location, FV) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xFV)
plotNormalHistogram(xFV2)
plotNormalHistogram(xFV3)
plotNormalHistogram(xFV4)
plotNormalHistogram(xFV5)

qqnorm(residuals(modelFV),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV),
       col="red")

qqnorm(residuals(modelFV2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV2),
       col="red")

qqnorm(residuals(modelFV3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV3),
       col="red")

qqnorm(residuals(modelFV4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV4),
       col="red")

qqnorm(residuals(modelFV5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFV5),
       col="red")

plot(fitted(modelFV),
     residuals(modelFV))

plot(fitted(modelFV2),
     residuals(modelFV2))

plot(fitted(modelFV3),
     residuals(modelFV3))

plot(fitted(modelFV4),
     residuals(modelFV4))


plot(fitted(modelFV5),
     residuals(modelFV5))
TUKEYFV<- summary(glht(modelFV, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYFV
# Assuming TUKEYFV is your summary object
significant_pairs <- sum(TUKEYFV$test$pvalues < 0.05)
total_pairs <- length(TUKEYFV$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")


tFV<- glht(modelFV3, linfct = mcp(location= "Tukey"))
t.cldFV <- cld(tFV)   # letter-based display
t.cldFV

#What is the effect of Euclidean distance on Adult weight in Festuca vaginata? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

FX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATAFRAME
FY=FX%>% 
  filter(Species=="Festuca vaginata")
FZ = FY %>%
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

#write.csv(FZ, "FV_Adult_meanSLA_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Festuca vaginata? 
#Open file
FD<-read_xlsx("FV_Distance_SLA.xlsx")
FD
#Best transformation
#No transformation
modelFD <- lm(Absolute~DISTANCE_KM , data=FD)
summary(modelFD)
Anova(modelFD, type="II")
xFD = (residuals(modelFD))
xFD
#Square root transformation
sqtransFD = sqrt(FD$Absolute)
modelFD2<-lm(sqtransFD~DISTANCE_KM, data=FD)
Anova(modelFD2, type="II") 
xFD2 = (residuals(modelFD2))
xFD2
#Log transformation
logtransFD = log(FD$Absolute)
modelFD3<-lm(logtransFD~DISTANCE_KM,  data=FD)
Anova(modelFD3, type="II") 
xFD3 = (residuals(modelFD3))
xFD3
#Cube root transformation
T_cubFD = sign(FD$Absolute) * abs(FD$Absolute)^(1/3)
modelFD4<-lm(T_cubFD~DISTANCE_KM,data=FD)
Anova(modelFD4, type="II")   ### THIS iS THE ONE 
xFD4 = (residuals(modelFD4))
xFD4
#Squaring transformation
squaringFD = (FD$Absolute)^2
modelFD5<-lm(squaringFD~DISTANCE_KM,  data=FD)
Anova(modelFD5, type="II")
xFD5 = (residuals(modelFD5))
xFD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFD)   #NT
shapiro.test(xFD2)  #Sqrt
shapiro.test(xFD3)  #Log
shapiro.test(xFD4)  #Cubert
shapiro.test(xFD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xFD)
plotNormalHistogram(xFD2)
plotNormalHistogram(xFD3)
plotNormalHistogram(xFD4)
plotNormalHistogram(xFD5)

qqnorm(residuals(modelFD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD),
       col="red")

qqnorm(residuals(modelFD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD2),
       col="red")

qqnorm(residuals(modelFD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD4),
       col="red")

qqnorm(residuals(modelFD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFD5),
       col="red")


plot(fitted(modelFD),
     residuals(modelFD))

plot(fitted(modelFD2),
     residuals(modelFD2))

plot(fitted(modelFD4),
     residuals(modelFD4))

plot(fitted(modelFD5),
     residuals(modelFD5))


#Distance- Absolute weight relationship plotting
##regression
plot(FD$DISTANCE_KM,T_cubFD, main="Connection between distance among locations on C.vaginata Adult weight",
     ylab="Adult weight difference betweeen locations (FRT) (g)", xlab="Distance (km)")
abline(lm(T_cubFD~FD$DISTANCE_KM), col="red") # regression line (y~x)
T_cubFD.lm = lm(T_cubFD~FD$DISTANCE_KM, data=FD)
summary(T_cubFD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Festuca vaginata? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
FV=data%>% 
  filter(Species=="Festuca vaginata")
FV$Location=as.factor(FV$Location)
FV$SLA=as.numeric(FV$SLA)
modelFZ <- lme(SLA ~ STZ, random = ~ 1 | Location, data = FV, na.action=na.omit)
modelFZ
Anova(modelFZ, type="II")
xFZ = (residuals(modelFZ))
xFZ
#Square root transformation
sqtransFZ = sqrt(FV$SLA)
modelFZ2<-lme(sqtransFZ~STZ,random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFZ2, type="II") ### THIS IS THE ONE
xFZ2 = (residuals(modelFZ2))
xFZ2
#Log transformation
logtransFZ = log(FV$SLA)
modelFZ3<-lme(logtransFZ~STZ,random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFZ3, type="II") 
xFZ3 = (residuals(modelFZ3)) 
xFZ3
#Cube root transformation
T_cubFZ = sign(FV$SLA) * abs(FV$SLA)^(1/3)
modelFZ4<-lme(T_cubFZ~STZ,random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFZ4, type="II")
xFZ4 = (residuals(modelFZ4))
xFZ4
#Squaring transformation
squaringFZ = (FV$SLA)^2
modelFV5<-lm(squaringFZ~STZ,random = ~ 1 | Location, data=FV, na.action=na.omit)
###CORRECTED FV5
Anova(modelFV5, type="II") 
xFZ5 = (residuals(modelFV5))
xFZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFZ)   #NT
shapiro.test(xFZ2)  #Sqrt
shapiro.test(xFZ3)  #Log
shapiro.test(xFZ4)  #Cubert
shapiro.test(xFZ5)  #Squaring
leveneTest(xFZ~STZ, FV) #NT
leveneTest(xFZ2~STZ, FV) #Sqrt
leveneTest(xFZ3~STZ, FV) #Log
leveneTest(xFZ4~STZ, FV) #Cubert
leveneTest(xFZ5~STZ, FV) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xFZ)
plotNormalHistogram(xFZ2)
plotNormalHistogram(xFZ3)
plotNormalHistogram(xFZ4)
plotNormalHistogram(xFZ5)

skewness(xFZ3)
kurtosis(xFZ3)
qqnorm(residuals(modelFZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelFZ3),
       col="red")
plot(fitted(modelFZ3),
     residuals(modelFZ))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Festuca vaginata? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
FV=data%>% 
  filter(Species=="Festuca vaginata")
FV$Location=as.factor(FV$Location)
FV$SLA=as.numeric(FV$SLA)
FV$Aridity=as.numeric(FV$Aridity)
modelFR <- lme(SLA~Aridity, random = ~ 1 | Location, data=FV, na.action=na.omit)
summary(modelFR)
Anova(modelFR, type="II")
xFR = (residuals(modelFR))
xFR
#Square root transformation
sqtransFR = sqrt(FV$SLA)
modelFR2<-lme(sqtransFR~Aridity,random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFR2, type="II") ### THIS IS THE ONE
xFR2 = (residuals(modelFR2))
xFR2
#Log transformation
logtransFR = log(FV$SLA)
modelFR3<-lme(logtransFR~Aridity, random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFR3, type="II")  
xFR3 = (residuals(modelFR3))
xFR3
#Cube root transformation
T_cubFR = sign(FV$SLA) * abs(FV$SLA)^(1/3)
modelFR4<-lme(T_cubFR~Aridity,random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFR4, type="II")
xFR4 = (residuals(modelFR4))
xFR4
#Squaring transformation
squaringFR = (FV$SLA)^2
modelFR5<-lme(squaringFR~Aridity, random = ~ 1 | Location, data=FV, na.action=na.omit)
Anova(modelFR5, type="II")
xFR5 = (residuals(modelFR5))
xFR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xFR)   #NT
shapiro.test(xFR2)  #Sqrt
shapiro.test(xFR3)  #Log
shapiro.test(xFR4)  #Cubert
shapiro.test(xFR5)  #Squaring
bartlett.test(FV$weight, FV$aridity)  #NT
bartlett.test(sqtransFR, FV$aridity)  #Sqrt
bartlett.test(logtransFR, FV$aridity) #Log
bartlett.test(T_cubFR, FV$aridity)    #Cubert
bartlett.test(squaringFR, FV$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xFR)
plotNormalHistogram(xFR2)
plotNormalHistogram(xFR3)
plotNormalHistogram(xFR4)
plotNormalHistogram(xFR5)
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
ggplot(FV, aes(x = Aridity, y = squaringFR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelFR5)))) 
#











