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
CA=data%>% 
  filter(Species=="Centaurea arenaria")
CA$Location=as.factor(CA$Location)
##Adult mass
###Location ~ shoot
#No transformation
modelCA<- lm(shoot~Location,data=CA)
modelCA
Anova(modelCA, type="II")
xCA = (residuals(modelCA))
xCA
#Square root transformation
sqtransCA = sqrt(CA$shoot)
modelCA2<-lm(sqtransCA~Location, data=CA)
Anova(modelCA2, type="II")
xCA2 = (residuals(modelCA2))
xCA2
#Log transformation
logtransCA = log(CA$shoot)
modelCA3<-lm(logtransCA~Location, data=CA)
Anova(modelCA3, type="II") ##### THIS IS THE ONE
xCA3 = (residuals(modelCA3))
xCA3

#Cube root transformation
T_cubCA = sign(CA$shoot) * abs(CA$shoot)^(1/3)
modelCA4<-lm(T_cubCA~Location, data=CA)
Anova(modelCA4, type="II")
xCA4 = (residuals(modelCA4))
xCA4
#Squaring transformation
squaringCA = (CA$shoot)^2
modelCA5<-lm(squaringCA~Location, data=CA)
##CORRECTED CA5
Anova(modelCA5, type="II") 
xCA5 = (residuals(modelCA5))
xCA5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCA)   #NT
shapiro.test(xCA2)  #Sqrt
shapiro.test(xCA3)  #Log
shapiro.test(xCA4)  #Cubert
shapiro.test(xCA5)  #Squaring

leveneTest(xCA~Location, CA) #NT
leveneTest(xCA2~Location, CA) #Sqrt
leveneTest(xCA3~Location, CA) #Log
leveneTest(xCA4~Location, CA) #Cubert
leveneTest(xCA5~Location, CA) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xCA)
plotNormalHistogram(xCA2)
plotNormalHistogram(xCA3)
plotNormalHistogram(xCA4)
plotNormalHistogram(xCA5)

qqnorm(residuals(modelCA),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA),
       col="red")

qqnorm(residuals(modelCA2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA2),
       col="red")

qqnorm(residuals(modelCA3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA3),
       col="red")

qqnorm(residuals(modelCA4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA4),
       col="red")

qqnorm(residuals(modelCA5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA5),
       col="red")

plot(fitted(modelCA),
     residuals(modelCA))

plot(fitted(modelCA2),
     residuals(modelCA2))

plot(fitted(modelCA3),
     residuals(modelCA3))

plot(fitted(modelCA4),
     residuals(modelCA4))


plot(fitted(modelCA5),
     residuals(modelCA5))
TUKEYCA<- summary(glht(modelCA3, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYCA
# Assuming TUKEYCA is your summary object
significant_pairs <- sum(TUKEYCA$test$pvalues < 0.05)
total_pairs <- length(TUKEYCA$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")


tCA<- glht(modelCA3, linfct = mcp(location= "Tukey"))
t.cldCA <- cld(tCA)   # letter-based display
t.cldCA

#What is the effect of Euclidean distance on Adult weight in Centaurea arenaria? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

CX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATACRAME
CY=CX%>% 
  filter(Species=="Centaurea arenaria")
CZ = CY %>%
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

write.csv(CZ, "CA_Adult_meanshoot_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Centaurea arenaria? 
#Open file
CD<-read_xlsx("CA_Distance_shoot.xlsx")
CD
#Best transformation
#No transformation
modelCD <- lm(Absolute~DISTANCE_KM , data=CD)
summary(modelCD)
Anova(modelCD, type="II")
xCD = (residuals(modelCD))
xCD
#Square root transformation
sqtransCD = sqrt(CD$Absolute)
modelCD2<-lm(sqtransCD~DISTANCE_KM, data=CD)
Anova(modelCD2, type="II") 
xCD2 = (residuals(modelCD2))
xCD2
#Log transformation
logtransCD = log(CD$Absolute)
modelCD3<-lm(logtransCD~DISTANCE_KM,  data=CD)
Anova(modelCD3, type="II") ### THIS iS THE ONE
xCD3 = (residuals(modelCD3))
xCD3
#Cube root transformation
T_cubCD = sign(CD$Absolute) * abs(CD$Absolute)^(1/3)
modelCD4<-lm(T_cubCD~DISTANCE_KM,data=CD)
Anova(modelCD4, type="II")    
xCD4 = (residuals(modelCD4))
xCD4
#Squaring transformation
squaringCD = (CD$Absolute)^2
modelCD5<-lm(squaringCD~DISTANCE_KM,  data=CD)
Anova(modelCD5, type="II")
xCD5 = (residuals(modelCD5))
xCD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCD)   #NT
shapiro.test(xCD2)  #Sqrt
shapiro.test(xCD3)  #Log
shapiro.test(xCD4)  #Cubert
shapiro.test(xCD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xCD)
plotNormalHistogram(xCD2)
plotNormalHistogram(xCD4)
plotNormalHistogram(xCD5)

qqnorm(residuals(modelCD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD),
       col="red")

qqnorm(residuals(modelCD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD2),
       col="red")

qqnorm(residuals(modelCD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD4),
       col="red")

qqnorm(residuals(modelCD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD5),
       col="red")

plot(fitted(modelCD),
     residuals(modelCD))

plot(fitted(modelCD2),
     residuals(modelCD2))

plot(fitted(modelCD4),
     residuals(modelCD4))

plot(fitted(modelCD5),
     residuals(modelCD5))


#Distance- Absolute weight relationship plotting
##regression
plot(CD$DISTANCE_KM,sqtransCD, main="Connection between distance among locations on C.arenaria Adult weight",
     ylab="Adult weight difference betweeen locations (CRT) (g)", xlab="Distance (km)")
abline(lm(sqtransCD~CD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransCD.lm = lm(sqtransCD~CD$DISTANCE_KM, data=CD)
summary(sqtransCD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Centaurea arenaria? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
CA=data%>% 
  filter(Species=="Centaurea arenaria")
CA$Location=as.factor(CA$Location)

modelCZ<- lme(shoot~STZ,random = ~ 1 | Location,data=CA)
modelCZ
Anova(modelCZ, type="II")
xCZ = (residuals(modelCZ))
xCZ
#Square root transformation
sqtransCZ = sqrt(CA$shoot)
modelCZ2<-lme(sqtransCZ~STZ,random = ~ 1 | Location, data=CA)
Anova(modelCZ2, type="II")
xCZ2 = (residuals(modelCZ2))
xCZ2
#Log transformation
logtransCZ = log(CA$shoot)
modelCZ3<-lme(logtransCZ~STZ,random = ~ 1 | Location, data=CA)
Anova(modelCZ3, type="II") 
xCZ3 = (residuals(modelCZ3)) ### THIS IS THE ONE
xCZ3
#Cube root transformation
T_cubCZ = sign(CA$shoot) * abs(CA$shoot)^(1/3)
modelCZ4<-lme(T_cubCZ~STZ,random = ~ 1 | Location, data=CA)
Anova(modelCZ4, type="II")
xCZ4 = (residuals(modelCZ4))
xCZ4
#Squaring transformation
squaringCZ = (CA$shoot)^2
##CORRECTED CA5
modelCA5<-lm(squaringCZ~STZ,random = ~ 1 | Location, data=CA)
Anova(modelCA5, type="II") 
xCZ5 = (residuals(modelCA5))
xCZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCZ)   #NT
shapiro.test(xCZ2)  #Sqrt
shapiro.test(xCZ3)  #Log
shapiro.test(xCZ4)  #Cubert
shapiro.test(xCZ5)  #Squaring
leveneTest(xCZ~STZ, CA) #NT
leveneTest(xCZ2~STZ, CA) #Sqrt
leveneTest(xCZ3~STZ, CA) #Log
leveneTest(xCZ4~STZ, CA) #Cubert
leveneTest(xCZ5~STZ, CA) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xCZ3)
skewness(xCZ3)
kurtosis(xCZ3)
qqnorm(residuals(modelCZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCZ3),
       col="red")
plot(fitted(modelCZ3),
     residuals(modelCZ))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Centaurea arenaria? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
CA=data%>% 
  filter(Species=="Centaurea arenaria")
CA$Location=as.factor(CA$Location)
modelCR <- lme(shoot~Aridity, random = ~ 1 | Location, data=CA)
summary(modelCR)
Anova(modelCR, type="II")
xCR = (residuals(modelCR))
xCR
#Square root transformation
sqtransCR = sqrt(CA$shoot)
modelCR2<-lme(sqtransCR~Aridity,random = ~ 1 | Location, data=CA)
Anova(modelCR2, type="II")
xCR2 = (residuals(modelCR2))
xCR2
#Log transformation
logtransCR = log(CA$shoot)
modelCR3<-lme(logtransCR~Aridity, random = ~ 1 | Location, data=CA)
Anova(modelCR3, type="II")  ### THIS IS THE ONE
xCR3 = (residuals(modelCR3))
xCR3
#Cube root transformation
T_cubCR = sign(CA$shoot) * abs(CA$shoot)^(1/3)
modelCR4<-lme(T_cubCR~Aridity,random = ~ 1 | Location, data=CA)
Anova(modelCR4, type="II")
xCR4 = (residuals(modelCR4))
xCR4
#Squaring transformation
squaringCR = (CA$shoot)^2
modelCR5<-lme(squaringCR~Aridity, random = ~ 1 | Location, data=CA)
Anova(modelCR5, type="II")
xCR5 = (residuals(modelCR5))
xCR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCR)   #NT
shapiro.test(xCR2)  #Sqrt
shapiro.test(xCR3)  #Log
shapiro.test(xCR4)  #Cubert
shapiro.test(xCR5)  #Squaring
bartlett.test(CA$weight, CA$aridity)  #NT
bartlett.test(sqtransCR, CA$aridity)  #Sqrt
bartlett.test(logtransCR, CA$aridity) #Log
bartlett.test(T_cubCR, CA$aridity)    #Cubert
bartlett.test(squaringCR, CA$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xCR3)
skewness(xCR3)
kurtosis(xCR3)
qqnorm(residuals(modelCR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCR3),
       col="red")
plot(fitted(modelCR3),
     residuals(modelCR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(CA, aes(x = Aridity, y = squaringCR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelCR5)))) 
#

##################################################################
################### ADULT SIZE ANALYSIS (R O O T)
##################
##################################################################

data<-read_xlsx("Adult_Masters.xlsx")
CA=data%>% 
  filter(Species=="Centaurea arenaria")
CA$Location=as.factor(CA$Location)
##Adult maximum leaf height
###Location ~ root
#No transformation
modelCA<- lm(root~Location,data=CA)
modelCA
Anova(modelCA, type="II")
xCA = (residuals(modelCA))
xCA
#Square root transformation
sqtransCA = sqrt(CA$root)
modelCA2<-lm(sqtransCA~Location, data=CA)
Anova(modelCA2, type="II")
xCA2 = (residuals(modelCA2))
xCA2
#Log transformation
logtransCA = log(CA$root)
modelCA3<-lm(logtransCA~Location, data=CA)
Anova(modelCA3, type="II")  ####### THIS IS THE ONE
xCA3 = (residuals(modelCA3))
xCA3

#Cube root transformation
T_cubCA = sign(CA$root) * abs(CA$root)^(1/3)
modelCA4<-lm(T_cubCA~Location, data=CA)
Anova(modelCA4, type="II") 
xCA4 = (residuals(modelCA4))
xCA4
#Squaring transformation
squaringCA = (CA$root)^2
modelCA5<-lm(squaringCA~Location, data=CA)
####CORRECTED MODEL CA5
Anova(modelCA5, type="II") 
xCA5 = (residuals(modelCA5))
xCA5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCA)   #NT
shapiro.test(xCA2)  #Sqrt
shapiro.test(xCA3)  #Log
shapiro.test(xCA4)  #Cubert
shapiro.test(xCA5)  #Squaring

leveneTest(xCA~Location, CA) #NT
leveneTest(xCA2~Location, CA) #Sqrt
leveneTest(xCA3~Location, CA) #Log
leveneTest(xCA4~Location, CA) #Cubert
leveneTest(xCA5~Location, CA) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xCA)
plotNormalHistogram(xCA2)
plotNormalHistogram(xCA3)
plotNormalHistogram(xCA4)
plotNormalHistogram(xCA5)

qqnorm(residuals(modelCA),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA),
       col="red")

qqnorm(residuals(modelCA2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA2),
       col="red")

qqnorm(residuals(modelCA3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA3),
       col="red")

qqnorm(residuals(modelCA4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA4),
       col="red")

qqnorm(residuals(modelCA5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA5),
       col="red")

plot(fitted(modelCA),
     residuals(modelCA))

plot(fitted(modelCA2),
     residuals(modelCA2))

plot(fitted(modelCA3),
     residuals(modelCA3))

plot(fitted(modelCA4),
     residuals(modelCA4))


plot(fitted(modelCA5),
     residuals(modelCA5))
TUKEYCA<- summary(glht(modelCA5, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYCA
# Assuming TUKEYCA is your summary object
significant_pairs <- sum(TUKEYCA$test$pvalues < 0.05)
total_pairs <- length(TUKEYCA$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")
tCA<- glht(modelCA3, linfct = mcp(location= "Tukey"))
t.cldCA <- cld(tCA)   # letter-based display
t.cldCA

#What is the effect of Ecological distance (STZ) on Adult weight in Centaurea arenaria? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

CX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATACRAME
CY=CX%>% 
  filter(Species=="Centaurea arenaria")
CZ=CY%>% 
  group_by(Location)%>% 
  summarize(
    avg_root=mean(root),
    avg_root=mean(root))

#write.csv(CZ, "meanroot_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Centaurea arenaria? 
#Open file
CD<-read_xlsx("CA_Distance_root.xlsx")
CD
#Best transformation
#No transformation
modelCD <- lm(Absolute~DISTANCE_KM , data=CD)
summary(modelCD)
Anova(modelCD, type="II")
xCD = (residuals(modelCD))
xCD
#Square root transformation
sqtransCD = sqrt(CD$Absolute)
modelCD2<-lm(sqtransCD~DISTANCE_KM, data=CD)
Anova(modelCD2, type="II") ### THIS iS THE ONE  
xCD2 = (residuals(modelCD2))
xCD2
#Log transformation
logtransCD = log(CD$Absolute)
modelCD3<-lm(logtransCD~DISTANCE_KM,  data=CD)
Anova(modelCD3, type="II")
xCD3 = (residuals(modelCD3))
xCD3
#Cube root transformation
T_cubCD = sign(CD$Absolute) * abs(CD$Absolute)^(1/3)
modelCD4<-lm(T_cubCD~DISTANCE_KM,data=CD)
Anova(modelCD4, type="II")  
xCD4 = (residuals(modelCD4))
xCD4
#Squaring transformation
squaringCD = (CD$Absolute)^2
modelCD5<-lm(squaringCD~DISTANCE_KM,  data=CD)
Anova(modelCD5, type="II")
xCD5 = (residuals(modelCD5))
xCD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCD)   #NT
shapiro.test(xCD2)  #Sqrt
shapiro.test(xCD3)  #Log
shapiro.test(xCD4)  #Cubert
shapiro.test(xCD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xCD)
plotNormalHistogram(xCD2)
plotNormalHistogram(xCD4)
plotNormalHistogram(xCD5)

qqnorm(residuals(modelCD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD),
       col="red")

qqnorm(residuals(modelCD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD2),
       col="red")

qqnorm(residuals(modelCD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD4),
       col="red")

qqnorm(residuals(modelCD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD5),
       col="red")


plot(fitted(modelCD),
     residuals(modelCD))

plot(fitted(modelCD2),
     residuals(modelCD2))

plot(fitted(modelCD4),
     residuals(modelCD4))

plot(fitted(modelCD5),
     residuals(modelCD5))


#Distance- Absolute weight relationship plotting
##regression
plot(CD$DISTANCE_KM,sqtransCD, main="Connection between distance among locations on C.arenaria Adult weight",
     ylab="Adult weight difference betweeen locations (CRT) (g)", xlab="Distance (km)")
abline(lm(sqtransCD~CD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransCD.lm = lm(sqtransCD~CD$DISTANCE_KM, data=CD)
summary(sqtransCD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Centaurea arenaria? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
CA=data%>% 
  filter(Species=="Centaurea arenaria")
CA$Location=as.factor(CA$Location)
CA$STZ=as.factor(CA$STZ)
modelCZ<- lme(root~STZ,random = ~ 1 | Location,data=CA)
modelCZ
Anova(modelCZ, type="II")
xCZ = (residuals(modelCZ))
xCZ
#Square root transformation
sqtransCZ = sqrt(CA$root)
modelCZ2<-lme(sqtransCZ~STZ,random = ~ 1 | Location, data=CA)
Anova(modelCZ2, type="II")
xCZ2 = (residuals(modelCZ2))
xCZ2
#Log transformation
logtransCZ = log(CA$root)
modelCZ3<-lme(logtransCZ~STZ,random = ~ 1 | Location, data=CA)
Anova(modelCZ3, type="II") ### THIS IS THE ONE
xCZ3 = (residuals(modelCZ3))
xCZ3
#Cube root transformation
T_cubCZ = sign(CA$root) * abs(CA$root)^(1/3)
modelCZ4<-lme(T_cubCZ~STZ,random = ~ 1 | Location, data=CA)
Anova(modelCZ4, type="II") 
xCZ4 = (residuals(modelCZ4))
xCZ4
#Squaring transformation
squaringCZ = (CA$root)^2
modelCA5<-lm(squaringCZ~STZ,random = ~ 1 | Location, data=CA)
Anova(modelCA5, type="II") 
xCZ5 = (residuals(modelCA5))
xCZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCZ)   #NT
shapiro.test(xCZ2)  #Sqrt
shapiro.test(xCZ3)  #Log
shapiro.test(xCZ4)  #Cubert
shapiro.test(xCZ5)  #Squaring
leveneTest(xCZ~STZ, CA) #NT
leveneTest(xCZ2~STZ, CA) #Sqrt
leveneTest(xCZ3~STZ, CA) #Log
leveneTest(xCZ4~STZ, CA) #Cubert
leveneTest(xCZ5~STZ, CA) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xCZ3)
skewness(xCZ3)
kurtosis(xCZ3)
qqnorm(residuals(modelCZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCZ3),
       col="red")
plot(fitted(modelCZ3),
     residuals(modelCZ))
TUKEYCZ<- summary(glht(modelCA5, linfct = mcp(STZ = "Tukey")), test = adjusted("holm"))
TUKEYCZ
# Assuming TUKEYCA is your summary object
significant_pairs <- sum(TUKEYCZ$test$pvalues < 0.05)
total_pairs <- length(TUKEYCZ$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")
#tCA<- glht(modelCA3, linfct = mcp(location= "Tukey"))
#t.cldCA <- cld(tCA)   # letter-based display
#t.cldCA

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Centaurea arenaria? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
CA=data%>% 
  filter(Species=="Centaurea arenaria")
CA$Location=as.factor(CA$Location)
modelCR <- lme(root~Aridity, random = ~ 1 | Location, data=CA)
summary(modelCR)
Anova(modelCR, type="II")
xCR = (residuals(modelCR))
xCR
#Square root transformation
sqtransCR = sqrt(CA$root)
modelCR2<-lme(sqtransCR~Aridity,random = ~ 1 | Location, data=CA)
Anova(modelCR2, type="II")
xCR2 = (residuals(modelCR2))
xCR2
#Log transformation
logtransCR = log(CA$root)
modelCR3<-lme(logtransCR~Aridity, random = ~ 1 | Location, data=CA)
Anova(modelCR3, type="II") ### THIS IS THE ONE
xCR3 = (residuals(modelCR3))
xCR3
#Cube root transformation
T_cubCR = sign(CA$root) * abs(CA$root)^(1/3)
modelCR4<-lme(T_cubCR~Aridity,random = ~ 1 | Location, data=CA)
Anova(modelCR4, type="II") 
xCR4 = (residuals(modelCR4))
xCR4
#Squaring transformation
squaringCR = (CA$root)^2
modelCR5<-lme(squaringCR~Aridity, random = ~ 1 | Location, data=CA)
Anova(modelCR5, type="II") ### THIS IS THE ONE
xCR5 = (residuals(modelCR5))
xCR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCR)   #NT
shapiro.test(xCR2)  #Sqrt
shapiro.test(xCR3)  #Log
shapiro.test(xCR4)  #Cubert
shapiro.test(xCR5)  #Squaring
bartlett.test(CA$weight, CA$aridity)  #NT
bartlett.test(sqtransCR, CA$aridity)  #Sqrt
bartlett.test(logtransCR, CA$aridity) #Log
bartlett.test(T_cubCR, CA$aridity)    #Cubert
bartlett.test(squaringCR, CA$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xCR3)
skewness(xCR3)
kurtosis(xCR3)
qqnorm(residuals(modelCR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCR3),
       col="red")
plot(fitted(modelCR3),
     residuals(modelCR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(CA, aes(x = Aridity, y = squaringCR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelCR5)))) 
#
######################################################
##########  LEAF   DRY  MATTER CONTENT ( L D M C)
###########
#####################################################

data<-read_xlsx("Adult_Masters.xlsx")
CA=data%>% 
  filter(Species=="Centaurea arenaria")
CA$Location=as.factor(CA$Location)
##Adult mass
###Location ~ LDMC
#No transformation
modelCA<- lm(LDMC~Location,data=CA)
modelCA
Anova(modelCA, type="II") 
xCA = (residuals(modelCA))
xCA
class(CA$LDMC)
CA$LDMC=as.numeric(CA$LDMC)
#Square root transformation
sqtransCA = sqrt(CA$LDMC)
modelCA2<-lm(sqtransCA~Location, data=CA)
Anova(modelCA2, type="II")
xCA2 = (residuals(modelCA2))
xCA2
#Log transformation
logtransCA = log(CA$LDMC)
modelCA3<-lm(logtransCA~Location, data=CA)
Anova(modelCA3, type="II") ##### THIS IS THE ONE
xCA3 = (residuals(modelCA3))
xCA3
#Cube root transformation
T_cubCA = sign(CA$LDMC) * abs(CA$LDMC)^(1/3)
modelCA4<-lm(T_cubCA~Location, data=CA)
Anova(modelCA4, type="II")
xCA4 = (residuals(modelCA4))
xCA4
#Squaring transformation
squaringCA = (CA$LDMC)^2
modelCA5<-lm(squaringCA~Location, data=CA)
####CORRECTED # CA5
Anova(modelCA5, type="II") 
xCA5 = (residuals(modelCA5))
xCA5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCA)   #NT
shapiro.test(xCA2)  #Sqrt
shapiro.test(xCA3)  #Log
shapiro.test(xCA4)  #Cubert
shapiro.test(xCA5)  #Squaring

leveneTest(xCA~Location, CA) #NT
leveneTest(xCA2~Location, CA) #Sqrt
leveneTest(xCA3~Location, CA) #Log
leveneTest(xCA4~Location, CA) #Cubert
leveneTest(xCA5~Location, CA) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xCA)
plotNormalHistogram(xCA2)
plotNormalHistogram(xCA3)
plotNormalHistogram(xCA4)
plotNormalHistogram(xCA5)

qqnorm(residuals(modelCA),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA),
       col="red")

qqnorm(residuals(modelCA2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA2),
       col="red")

qqnorm(residuals(modelCA3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA3),
       col="red")

qqnorm(residuals(modelCA4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA4),
       col="red")

qqnorm(residuals(modelCA5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA5),
       col="red")

plot(fitted(modelCA),
     residuals(modelCA))

plot(fitted(modelCA2),
     residuals(modelCA2))

plot(fitted(modelCA3),
     residuals(modelCA3))

plot(fitted(modelCA4),
     residuals(modelCA4))


plot(fitted(modelCA5),
     residuals(modelCA5))
TUKEYCA<- summary(glht(modelCA3, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYCA
# Assuming TUKEYCA is your summary object
significant_pairs <- sum(TUKEYCA$test$pvalues < 0.05)
total_pairs <- length(TUKEYCA$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")


tCA<- glht(modelCA3, linfct = mcp(location= "Tukey"))
t.cldCA <- cld(tCA)   # letter-based display
t.cldCA

#What is the effect of Euclidean distance on Adult weight in Centaurea arenaria? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

CX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATACRAME
CY=CX%>% 
  filter(Species=="Centaurea arenaria")
CZ = CY %>%
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

write.csv(CZ, "CA_Adult_meanLDMC_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Centaurea arenaria? 
#Open file
CD<-read_xlsx("CA_Distance_LDMC.xlsx")
CD
#Best transformation
#No transformation
modelCD <- lm(Absolute~DISTANCE_KM , data=CD)
summary(modelCD)
Anova(modelCD, type="II")
xCD = (residuals(modelCD))
xCD
#Square root transformation
sqtransCD = sqrt(CD$Absolute)
modelCD2<-lm(sqtransCD~DISTANCE_KM, data=CD)
Anova(modelCD2, type="II") ### THIS iS THE ONE
xCD2 = (residuals(modelCD2))
xCD2
#Log transformation
logtransCD = log(CD$Absolute)
modelCD3<-lm(logtransCD~DISTANCE_KM,  data=CD)
Anova(modelCD3, type="II") 
xCD3 = (residuals(modelCD3))
xCD3
#Cube root transformation
T_cubCD = sign(CD$Absolute) * abs(CD$Absolute)^(1/3)
modelCD4<-lm(T_cubCD~DISTANCE_KM,data=CD)
Anova(modelCD4, type="II")    
xCD4 = (residuals(modelCD4))
xCD4
#Squaring transformation
squaringCD = (CD$Absolute)^2
modelCD5<-lm(squaringCD~DISTANCE_KM,  data=CD)
Anova(modelCD5, type="II")
xCD5 = (residuals(modelCD5))
xCD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCD)   #NT
shapiro.test(xCD2)  #Sqrt
shapiro.test(xCD3)  #Log
shapiro.test(xCD4)  #Cubert
shapiro.test(xCD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xCD)
plotNormalHistogram(xCD2)
plotNormalHistogram(xCD4)
plotNormalHistogram(xCD5)

qqnorm(residuals(modelCD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD),
       col="red")

qqnorm(residuals(modelCD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD2),
       col="red")

qqnorm(residuals(modelCD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD4),
       col="red")

qqnorm(residuals(modelCD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD5),
       col="red")


plot(fitted(modelCD),
     residuals(modelCD))

plot(fitted(modelCD2),
     residuals(modelCD2))

plot(fitted(modelCD4),
     residuals(modelCD4))

plot(fitted(modelCD5),
     residuals(modelCD5))


#Distance- Absolute weight relationship plotting
##regression
plot(CD$DISTANCE_KM,sqtransCD, main="Connection between distance among locations on C.arenaria Adult weight",
     ylab="Adult weight difference betweeen locations (CRT) (g)", xlab="Distance (km)")
abline(lm(sqtransCD~CD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransCD.lm = lm(sqtransCD~CD$DISTANCE_KM, data=CD)
summary(sqtransCD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Centaurea arenaria? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
CA=data%>% 
  filter(Species=="Centaurea arenaria")
CA$Location=as.factor(CA$Location)
CA$LDMC=as.numeric(CA$LDMC)
modelCZ <- lme(LDMC ~ STZ, random = ~ 1 | Location, data = CA, na.action=na.omit)
modelCZ
Anova(modelCZ, type="II")
xCZ = (residuals(modelCZ))
xCZ
#Square root transformation
sqtransCZ = sqrt(CA$LDMC)
modelCZ2<-lme(sqtransCZ~STZ,random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCZ2, type="II") ### THIS IS THE ONE
xCZ2 = (residuals(modelCZ2))
xCZ2
#Log transformation
logtransCZ = log(CA$LDMC)
modelCZ3<-lme(logtransCZ~STZ,random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCZ3, type="II") 
xCZ3 = (residuals(modelCZ3)) 
xCZ3
#Cube root transformation
T_cubCZ = sign(CA$LDMC) * abs(CA$LDMC)^(1/3)
modelCZ4<-lme(T_cubCZ~STZ,random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCZ4, type="II")
xCZ4 = (residuals(modelCZ4))
xCZ4
#Squaring transformation
squaringCZ = (CA$LDMC)^2
modelCA5<-lm(squaringCZ~STZ,random = ~ 1 | Location, data=CA, na.action=na.omit)
##CORRECTED MODEL CA5
Anova(modelCA5, type="II") 
xCZ5 = (residuals(modelCA5))
xCZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCZ)   #NT
shapiro.test(xCZ2)  #Sqrt
shapiro.test(xCZ3)  #Log
shapiro.test(xCZ4)  #Cubert
shapiro.test(xCZ5)  #Squaring
leveneTest(xCZ~STZ, CA) #NT
leveneTest(xCZ2~STZ, CA) #Sqrt
leveneTest(xCZ3~STZ, CA) #Log
leveneTest(xCZ4~STZ, CA) #Cubert
leveneTest(xCZ5~STZ, CA) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xCZ3)
skewness(xCZ3)
kurtosis(xCZ3)
qqnorm(residuals(modelCZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCZ3),
       col="red")
plot(fitted(modelCZ3),
     residuals(modelCZ))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Centaurea arenaria? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
CA=data%>% 
  filter(Species=="Centaurea arenaria")
CA$Location=as.factor(CA$Location)
CA$LDMC=as.numeric(CA$LDMC)
CA$Aridity=as.numeric(CA$Aridity)
modelCR <- lme(LDMC~Aridity, random = ~ 1 | Location, data=CA, na.action=na.omit)
summary(modelCR)
Anova(modelCR, type="II")
xCR = (residuals(modelCR))
xCR
#Square root transformation
sqtransCR = sqrt(CA$LDMC)
modelCR2<-lme(sqtransCR~Aridity,random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCR2, type="II") ### THIS IS THE ONE
xCR2 = (residuals(modelCR2))
xCR2
#Log transformation
logtransCR = log(CA$LDMC)
modelCR3<-lme(logtransCR~Aridity, random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCR3, type="II")  
xCR3 = (residuals(modelCR3))
xCR3
#Cube root transformation
T_cubCR = sign(CA$LDMC) * abs(CA$LDMC)^(1/3)
modelCR4<-lme(T_cubCR~Aridity,random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCR4, type="II")
xCR4 = (residuals(modelCR4))
xCR4
#Squaring transformation
squaringCR = (CA$LDMC)^2
modelCR5<-lme(squaringCR~Aridity, random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCR5, type="II")
xCR5 = (residuals(modelCR5))
xCR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCR)   #NT
shapiro.test(xCR2)  #Sqrt
shapiro.test(xCR3)  #Log
shapiro.test(xCR4)  #Cubert
shapiro.test(xCR5)  #Squaring
bartlett.test(CA$weight, CA$aridity)  #NT
bartlett.test(sqtransCR, CA$aridity)  #Sqrt
bartlett.test(logtransCR, CA$aridity) #Log
bartlett.test(T_cubCR, CA$aridity)    #Cubert
bartlett.test(squaringCR, CA$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xCR3)
skewness(xCR3)
kurtosis(xCR3)
qqnorm(residuals(modelCR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCR3),
       col="red")
plot(fitted(modelCR3),
     residuals(modelCR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(CA, aes(x = Aridity, y = squaringCR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelCR5)))) 
#

######################################################
########## SPECIFIC LEAF AREA (S  L  A)
###########
#####################################################
data<-read_xlsx("Adult_Masters.xlsx")
CA=data%>% 
  filter(Species=="Centaurea arenaria")
CA$Location=as.factor(CA$Location)
##Adult mass
###Location ~ SLA
#No transformation
modelCA<- lm(SLA~Location,data=CA)
modelCA
Anova(modelCA, type="II") ##### THIS IS THE ONE
xCA = (residuals(modelCA))
xCA
class(CA$SLA)
CA$SLA=as.numeric(CA$SLA)
#Square root transformation
sqtransCA = sqrt(CA$SLA)
modelCA2<-lm(sqtransCA~Location, data=CA)
Anova(modelCA2, type="II")
xCA2 = (residuals(modelCA2))
xCA2
#Log transformation
logtransCA = log(CA$SLA)
modelCA3<-lm(logtransCA~Location, data=CA)
Anova(modelCA3, type="II") 
xCA3 = (residuals(modelCA3))
xCA3
#Cube root transformation
T_cubCA = sign(CA$SLA) * abs(CA$SLA)^(1/3)
modelCA4<-lm(T_cubCA~Location, data=CA)
Anova(modelCA4, type="II")
xCA4 = (residuals(modelCA4))
xCA4
#Squaring transformation
squaringCA = (CA$SLA)^2
modelCA5<-lm(squaringCA~Location, data=CA)
##CORRECTED MODEL CA5
Anova(modelCA5, type="II") 
xCA5 = (residuals(modelCA5))
xCA5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCA)   #NT
shapiro.test(xCA2)  #Sqrt
shapiro.test(xCA3)  #Log
shapiro.test(xCA4)  #Cubert
shapiro.test(xCA5)  #Squaring

leveneTest(xCA~Location, CA) #NT
leveneTest(xCA2~Location, CA) #Sqrt
leveneTest(xCA3~Location, CA) #Log
leveneTest(xCA4~Location, CA) #Cubert
leveneTest(xCA5~Location, CA) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xCA)
plotNormalHistogram(xCA2)
plotNormalHistogram(xCA3)
plotNormalHistogram(xCA4)
plotNormalHistogram(xCA5)

qqnorm(residuals(modelCA),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA),
       col="red")

qqnorm(residuals(modelCA2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA2),
       col="red")

qqnorm(residuals(modelCA3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA3),
       col="red")

qqnorm(residuals(modelCA4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA4),
       col="red")

qqnorm(residuals(modelCA5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCA5),
       col="red")

plot(fitted(modelCA),
     residuals(modelCA))

plot(fitted(modelCA2),
     residuals(modelCA2))

plot(fitted(modelCA3),
     residuals(modelCA3))

plot(fitted(modelCA4),
     residuals(modelCA4))


plot(fitted(modelCA5),
     residuals(modelCA5))
TUKEYCA<- summary(glht(modelCA, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYCA
# Assuming TUKEYCA is your summary object
significant_pairs <- sum(TUKEYCA$test$pvalues < 0.05)
total_pairs <- length(TUKEYCA$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")


tCA<- glht(modelCA3, linfct = mcp(location= "Tukey"))
t.cldCA <- cld(tCA)   # letter-based display
t.cldCA

#What is the effect of Euclidean distance on Adult weight in Centaurea arenaria? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

CX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATACRAME
CY=CX%>% 
  filter(Species=="Centaurea arenaria")
CZ = CY %>%
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

#write.csv(CZ, "CA_Adult_meanSLA_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Centaurea arenaria? 
#Open file
CD<-read_xlsx("CA_Distance_SLA.xlsx")
CD
#Best transformation
#No transformation
modelCD <- lm(Absolute~DISTANCE_KM , data=CD)
summary(modelCD)
Anova(modelCD, type="II")
xCD = (residuals(modelCD))
xCD
#Square root transformation
sqtransCD = sqrt(CD$Absolute)
modelCD2<-lm(sqtransCD~DISTANCE_KM, data=CD)
Anova(modelCD2, type="II") ### THIS iS THE ONE
xCD2 = (residuals(modelCD2))
xCD2
#Log transformation
logtransCD = log(CD$Absolute)
modelCD3<-lm(logtransCD~DISTANCE_KM,  data=CD)
Anova(modelCD3, type="II") 
xCD3 = (residuals(modelCD3))
xCD3
#Cube root transformation
T_cubCD = sign(CD$Absolute) * abs(CD$Absolute)^(1/3)
modelCD4<-lm(T_cubCD~DISTANCE_KM,data=CD)
Anova(modelCD4, type="II")    
xCD4 = (residuals(modelCD4))
xCD4
#Squaring transformation
squaringCD = (CD$Absolute)^2
modelCD5<-lm(squaringCD~DISTANCE_KM,  data=CD)
Anova(modelCD5, type="II")
xCD5 = (residuals(modelCD5))
xCD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCD)   #NT
shapiro.test(xCD2)  #Sqrt
shapiro.test(xCD3)  #Log
shapiro.test(xCD4)  #Cubert
shapiro.test(xCD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xCD)
plotNormalHistogram(xCD2)
plotNormalHistogram(xCD4)
plotNormalHistogram(xCD5)

qqnorm(residuals(modelCD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD),
       col="red")

qqnorm(residuals(modelCD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD2),
       col="red")

qqnorm(residuals(modelCD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD4),
       col="red")

qqnorm(residuals(modelCD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCD5),
       col="red")


plot(fitted(modelCD),
     residuals(modelCD))

plot(fitted(modelCD2),
     residuals(modelCD2))

plot(fitted(modelCD4),
     residuals(modelCD4))

plot(fitted(modelCD5),
     residuals(modelCD5))


#Distance- Absolute weight relationship plotting
##regression
plot(CD$DISTANCE_KM,sqtransCD, main="Connection between distance among locations on C.arenaria Adult weight",
     ylab="Adult weight difference betweeen locations (CRT) (g)", xlab="Distance (km)")
abline(lm(sqtransCD~CD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransCD.lm = lm(sqtransCD~CD$DISTANCE_KM, data=CD)
summary(sqtransCD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Centaurea arenaria? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
CA=data%>% 
  filter(Species=="Centaurea arenaria")
CA$Location=as.factor(CA$Location)
CA$SLA=as.numeric(CA$SLA)
modelCZ <- lme(SLA ~ STZ, random = ~ 1 | Location, data = CA, na.action=na.omit)
modelCZ
Anova(modelCZ, type="II")
xCZ = (residuals(modelCZ))
xCZ
#Square root transformation
sqtransCZ = sqrt(CA$SLA)
modelCZ2<-lme(sqtransCZ~STZ,random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCZ2, type="II") ### THIS IS THE ONE
xCZ2 = (residuals(modelCZ2))
xCZ2
#Log transformation
logtransCZ = log(CA$SLA)
modelCZ3<-lme(logtransCZ~STZ,random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCZ3, type="II") 
xCZ3 = (residuals(modelCZ3)) 
xCZ3
#Cube root transformation
T_cubCZ = sign(CA$SLA) * abs(CA$SLA)^(1/3)
modelCZ4<-lme(T_cubCZ~STZ,random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCZ4, type="II")
xCZ4 = (residuals(modelCZ4))
xCZ4
#Squaring transformation
squaringCZ = (CA$SLA)^2
modelCA5<-lm(squaringCZ~STZ,random = ~ 1 | Location, data=CA, na.action=na.omit)
#CORRECTED MODEL CA5
Anova(modelCA5, type="II") 
xCZ5 = (residuals(modelCA5))
xCZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCZ)   #NT
shapiro.test(xCZ2)  #Sqrt
shapiro.test(xCZ3)  #Log
shapiro.test(xCZ4)  #Cubert
shapiro.test(xCZ5)  #Squaring
leveneTest(xCZ~STZ, CA) #NT
leveneTest(xCZ2~STZ, CA) #Sqrt
leveneTest(xCZ3~STZ, CA) #Log
leveneTest(xCZ4~STZ, CA) #Cubert
leveneTest(xCZ5~STZ, CA) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xCZ3)
skewness(xCZ3)
kurtosis(xCZ3)
qqnorm(residuals(modelCZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCZ3),
       col="red")
plot(fitted(modelCZ3),
     residuals(modelCZ))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Centaurea arenaria? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
CA=data%>% 
  filter(Species=="Centaurea arenaria")
CA$Location=as.factor(CA$Location)
CA$SLA=as.numeric(CA$SLA)
CA$Aridity=as.numeric(CA$Aridity)
modelCR <- lme(SLA~Aridity, random = ~ 1 | Location, data=CA, na.action=na.omit)
summary(modelCR)
Anova(modelCR, type="II")
xCR = (residuals(modelCR))
xCR
#Square root transformation
sqtransCR = sqrt(CA$SLA)
modelCR2<-lme(sqtransCR~Aridity,random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCR2, type="II") ### THIS IS THE ONE
xCR2 = (residuals(modelCR2))
xCR2
#Log transformation
logtransCR = log(CA$SLA)
modelCR3<-lme(logtransCR~Aridity, random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCR3, type="II")  
xCR3 = (residuals(modelCR3))
xCR3
#Cube root transformation
T_cubCR = sign(CA$SLA) * abs(CA$SLA)^(1/3)
modelCR4<-lme(T_cubCR~Aridity,random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCR4, type="II")
xCR4 = (residuals(modelCR4))
xCR4
#Squaring transformation
squaringCR = (CA$SLA)^2
modelCR5<-lme(squaringCR~Aridity, random = ~ 1 | Location, data=CA, na.action=na.omit)
Anova(modelCR5, type="II")
xCR5 = (residuals(modelCR5))
xCR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xCR)   #NT
shapiro.test(xCR2)  #Sqrt
shapiro.test(xCR3)  #Log
shapiro.test(xCR4)  #Cubert
shapiro.test(xCR5)  #Squaring
bartlett.test(CA$weight, CA$aridity)  #NT
bartlett.test(sqtransCR, CA$aridity)  #Sqrt
bartlett.test(logtransCR, CA$aridity) #Log
bartlett.test(T_cubCR, CA$aridity)    #Cubert
bartlett.test(squaringCR, CA$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xCR3)
skewness(xCR3)
kurtosis(xCR3)
qqnorm(residuals(modelCR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelCR3),
       col="red")
plot(fitted(modelCR3),
     residuals(modelCR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(CA, aes(x = Aridity, y = squaringCR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelCR5)))) 
#











