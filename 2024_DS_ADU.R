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
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
##Adult mass
###Location ~ shoot
#No transformation
modelDS<- lm(shoot~Location,data=DS)
modelDS
Anova(modelDS, type="II")
xDS = (residuals(modelDS))
xDS
#Square root transformation
sqtransDS = sqrt(DS$shoot)
modelDS2<-lm(sqtransDS~Location, data=DS)
Anova(modelDS2, type="II")
xDS2 = (residuals(modelDS2))
xDS2
#Log transformation
logtransDS = log(DS$shoot)
modelDS3<-lm(logtransDS~Location, data=DS)
Anova(modelDS3, type="II") ##### THIS IS THE ONE
xDS3 = (residuals(modelDS3))
xDS3

#Cube root transformation
T_cubDS = sign(DS$shoot) * abs(DS$shoot)^(1/3)
modelDS4<-lm(T_cubDS~Location, data=DS)
Anova(modelDS4, type="II")
xDS4 = (residuals(modelDS4))
xDS4
#Squaring transformation
squaringDS = (DS$shoot)^2
modelDS5<-lm(squaringDS~Location, data=DS)
###MODELDS5
Anova(modelDS5, type="II") 
xDS5 = (residuals(modelDS5))
xDS5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDS)   #NT
shapiro.test(xDS2)  #Sqrt
shapiro.test(xDS3)  #Log
shapiro.test(xDS4)  #Cubert
shapiro.test(xDS5)  #Squaring

leveneTest(xDS~Location, DS) #NT
leveneTest(xDS2~Location, DS) #Sqrt
leveneTest(xDS3~Location, DS) #Log
leveneTest(xDS4~Location, DS) #Cubert
leveneTest(xDS5~Location, DS) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xDS)
plotNormalHistogram(xDS2)
plotNormalHistogram(xDS3)
plotNormalHistogram(xDS4)
plotNormalHistogram(xDS5)

qqnorm(residuals(modelDS),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS),
       col="red")

qqnorm(residuals(modelDS2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS2),
       col="red")

qqnorm(residuals(modelDS3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS3),
       col="red")

qqnorm(residuals(modelDS4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS4),
       col="red")

qqnorm(residuals(modelDS5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS5),
       col="red")

plot(fitted(modelDS),
     residuals(modelDS))

plot(fitted(modelDS2),
     residuals(modelDS2))

plot(fitted(modelDS3),
     residuals(modelDS3))

plot(fitted(modelDS4),
     residuals(modelDS4))


plot(fitted(modelDS5),
     residuals(modelDS5))
TUKEYDS<- summary(glht(modelDS3, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYDS
# Assuming TUKEYDS is your summary object
significant_pairs <- sum(TUKEYDS$test$pvalues < 0.05)
total_pairs <- length(TUKEYDS$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")


tDS<- glht(modelDS3, linfct = mcp(location= "Tukey"))
t.cldDS <- cld(tDS)   # letter-based display
t.cldDS

#What is the effect of Euclidean distance on Adult weight in Dianthus serotinus? 
#Best transformation
# for distance firstly we calcuated the mean by location and the make the table for calculation

DX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATADRAME
DY=DX%>% 
  filter(Species=="Dianthus serotinus")
DZ = DY %>%
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

write.csv(DZ, "DS_Adult_meanshoot_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Dianthus serotinus? 
#Open file
DD<-read_xlsx("DS_Distance_shoot.xlsx")
DD
#Best transformation
#No transformation
modelDD <- lm(Absolute~DISTANCE_KM , data=DD)
summary(modelDD)
Anova(modelDD, type="II")
xDD = (residuals(modelDD))
xDD
#Square root transformation
sqtransDD = sqrt(DD$Absolute)
modelDD2<-lm(sqtransDD~DISTANCE_KM, data=DD)
Anova(modelDD2, type="II") 
xDD2 = (residuals(modelDD2))
xDD2
#Log transformation
logtransDD = log(DD$Absolute)
modelDD3<-lm(logtransDD~DISTANCE_KM,  data=DD)
Anova(modelDD3, type="II") 
xDD3 = (residuals(modelDD3))
xDD3
#Cube root transformation
T_cubDD = sign(DD$Absolute) * abs(DD$Absolute)^(1/3)
modelDD4<-lm(T_cubDD~DISTANCE_KM,data=DD)
Anova(modelDD4, type="II")  ### THIS iS THE ONE   
xDD4 = (residuals(modelDD4))
xDD4
#Squaring transformation
squaringDD = (DD$Absolute)^2
modelDD5<-lm(squaringDD~DISTANCE_KM,  data=DD)
Anova(modelDD5, type="II")
xDD5 = (residuals(modelDD5))
xDD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDD)   #NT
shapiro.test(xDD2)  #Sqrt
shapiro.test(xDD3)  #Log
shapiro.test(xDD4)  #Cubert
shapiro.test(xDD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xDD)
plotNormalHistogram(xDD2)
plotNormalHistogram(xDD4)
plotNormalHistogram(xDD5)

qqnorm(residuals(modelDD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD),
       col="red")

qqnorm(residuals(modelDD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD2),
       col="red")

qqnorm(residuals(modelDD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD4),
       col="red")

qqnorm(residuals(modelDD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD5),
       col="red")


plot(fitted(modelDD),
     residuals(modelDD))

plot(fitted(modelDD2),
     residuals(modelDD2))

plot(fitted(modelDD4),
     residuals(modelDD4))

plot(fitted(modelDD5),
     residuals(modelDD5))


#Distance- Absolute weight relationship plotting
##regression
plot(DD$DISTANCE_KM,sqtransDD, main="Connection between distance among locations on C.serotinus Adult weight",
     ylab="Adult weight difference betweeen locations (DRT) (g)", xlab="Distance (km)")
abline(lm(sqtransDD~DD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransDD.lm = lm(sqtransDD~DD$DISTANCE_KM, data=DD)
summary(sqtransDD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Dianthus serotinus? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)

modelDZ<- lme(shoot~STZ,random = ~ 1 | Location,data=DS)
modelDZ
Anova(modelDZ, type="II")
xDZ = (residuals(modelDZ))
xDZ
#Square root transformation
sqtransDZ = sqrt(DS$shoot)
modelDZ2<-lme(sqtransDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDZ2, type="II")
xDZ2 = (residuals(modelDZ2))
xDZ2
#Log transformation
logtransDZ = log(DS$shoot)
modelDZ3<-lme(logtransDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDZ3, type="II") ### THIS IS THE ONE
xDZ3 = (residuals(modelDZ3)) 
xDZ3
#Cube root transformation
T_cubDZ = sign(DS$shoot) * abs(DS$shoot)^(1/3)
modelDZ4<-lme(T_cubDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDZ4, type="II")
xDZ4 = (residuals(modelDZ4))
xDZ4
#Squaring transformation
squaringDZ = (DS$shoot)^2
modelDS5<-lm(squaringDZ~STZ,random = ~ 1 | Location, data=DS)
###MODEL DS5
Anova(modelDS5, type="II") 
xDZ5 = (residuals(modelDS5))
xDZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDZ)   #NT
shapiro.test(xDZ2)  #Sqrt
shapiro.test(xDZ3)  #Log
shapiro.test(xDZ4)  #Cubert
shapiro.test(xDZ5)  #Squaring
leveneTest(xDZ~STZ, DS) #NT
leveneTest(xDZ2~STZ, DS) #Sqrt
leveneTest(xDZ3~STZ, DS) #Log
leveneTest(xDZ4~STZ, DS) #Cubert
leveneTest(xDZ5~STZ, DS) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xDZ3)
skewness(xDZ3)
kurtosis(xDZ3)
qqnorm(residuals(modelDZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDZ3),
       col="red")
plot(fitted(modelDZ3),
     residuals(modelDZ))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Dianthus serotinus? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
modelDR <- lme(shoot~Aridity, random = ~ 1 | Location, data=DS)
summary(modelDR)
Anova(modelDR, type="II")
xDR = (residuals(modelDR))
xDR
#Square root transformation
sqtransDR = sqrt(DS$shoot)
modelDR2<-lme(sqtransDR~Aridity,random = ~ 1 | Location, data=DS)
Anova(modelDR2, type="II")
xDR2 = (residuals(modelDR2))
xDR2
#Log transformation
logtransDR = log(DS$shoot)
modelDR3<-lme(logtransDR~Aridity, random = ~ 1 | Location, data=DS)
Anova(modelDR3, type="II")  ### THIS IS THE ONE
xDR3 = (residuals(modelDR3))
xDR3
#Cube root transformation
T_cubDR = sign(DS$shoot) * abs(DS$shoot)^(1/3)
modelDR4<-lme(T_cubDR~Aridity,random = ~ 1 | Location, data=DS)
Anova(modelDR4, type="II")
xDR4 = (residuals(modelDR4))
xDR4
#Squaring transformation
squaringDR = (DS$shoot)^2
modelDR5<-lme(squaringDR~Aridity, random = ~ 1 | Location, data=DS)
Anova(modelDR5, type="II")
xDR5 = (residuals(modelDR5))
xDR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDR)   #NT
shapiro.test(xDR2)  #Sqrt
shapiro.test(xDR3)  #Log
shapiro.test(xDR4)  #Cubert
shapiro.test(xDR5)  #Squaring
bartlett.test(DS$weight, DS$aridity)  #NT
bartlett.test(sqtransDR, DS$aridity)  #Sqrt
bartlett.test(logtransDR, DS$aridity) #Log
bartlett.test(T_cubDR, DS$aridity)    #Cubert
bartlett.test(squaringDR, DS$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xDR3)
skewness(xDR3)
kurtosis(xDR3)
qqnorm(residuals(modelDR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDR3),
       col="red")
plot(fitted(modelDR3),
     residuals(modelDR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(DS, aes(x = Aridity, y = squaringDR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelDR5)))) 
#

##################################################################
################### ADULT SIZE ANALYSIS (R O O T)
##################
##################################################################

data<-read_xlsx("Adult_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
##Adult maximum leaf height
###Location ~ root
#No transformation
modelDS<- lm(root~Location,data=DS)
modelDS
Anova(modelDS, type="II")
xDS = (residuals(modelDS))
xDS
#Square root transformation
sqtransDS = sqrt(DS$root)
modelDS2<-lm(sqtransDS~Location, data=DS)
Anova(modelDS2, type="II")
xDS2 = (residuals(modelDS2))
xDS2
#Log transformation
logtransDS = log(DS$root)
modelDS3<-lm(logtransDS~Location, data=DS)
Anova(modelDS3, type="II")  ####### THIS IS THE ONE
xDS3 = (residuals(modelDS3))
xDS3

#Cube root transformation
T_cubDS = sign(DS$root) * abs(DS$root)^(1/3)
modelDS4<-lm(T_cubDS~Location, data=DS)
Anova(modelDS4, type="II") 
xDS4 = (residuals(modelDS4))
xDS4
#Squaring transformation
squaringDS = (DS$root)^2
modelDS5<-lm(squaringDS~Location, data=DS)
#####MODEL DS5
Anova(modelDS5, type="II") 
xDS5 = (residuals(modelDS5))
xDS5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDS)   #NT
shapiro.test(xDS2)  #Sqrt
shapiro.test(xDS3)  #Log
shapiro.test(xDS4)  #Cubert
shapiro.test(xDS5)  #Squaring

leveneTest(xDS~Location, DS) #NT
leveneTest(xDS2~Location, DS) #Sqrt
leveneTest(xDS3~Location, DS) #Log
leveneTest(xDS4~Location, DS) #Cubert
leveneTest(xDS5~Location, DS) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xDS)
plotNormalHistogram(xDS2)
plotNormalHistogram(xDS3)
plotNormalHistogram(xDS4)
plotNormalHistogram(xDS5)

qqnorm(residuals(modelDS),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS),
       col="red")

qqnorm(residuals(modelDS2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS2),
       col="red")

qqnorm(residuals(modelDS3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS3),
       col="red")

qqnorm(residuals(modelDS4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS4),
       col="red")

qqnorm(residuals(modelDS5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS5),
       col="red")

plot(fitted(modelDS),
     residuals(modelDS))

plot(fitted(modelDS2),
     residuals(modelDS2))

plot(fitted(modelDS3),
     residuals(modelDS3))

plot(fitted(modelDS4),
     residuals(modelDS4))


plot(fitted(modelDS5),
     residuals(modelDS5))
TUKEYDS<- summary(glht(modelDS3, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYDS
# Assuming TUKEYDS is your summary object
significant_pairs <- sum(TUKEYDS$test$pvalues < 0.05)
total_pairs <- length(TUKEYDS$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")
tDS<- glht(modelDS3, linfct = mcp(location= "Tukey"))
t.cldDS <- cld(tDS)   # letter-based display
t.cldDS

#What is the effect of Ecological distance (STZ) on Adult weight in Dianthus serotinus? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

DX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATADRAME
DY=DX%>% 
  filter(Species=="Dianthus serotinus")
DZ=DY%>% 
  group_by(Location)%>% 
  summarize(
    avg_root=mean(root),
    avg_root=mean(root))

#write.csv(DZ, "meanroot_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Dianthus serotinus? 
#Open file
DD<-read_xlsx("DS_Distance_root.xlsx")
DD
#Best transformation
#No transformation
modelDD <- lm(Absolute~DISTANCE_KM , data=DD)
summary(modelDD)
Anova(modelDD, type="II")
xDD = (residuals(modelDD))
xDD
#Square root transformation
sqtransDD = sqrt(DD$Absolute)
modelDD2<-lm(sqtransDD~DISTANCE_KM, data=DD)
Anova(modelDD2, type="II")  
xDD2 = (residuals(modelDD2))
xDD2
#Log transformation
logtransDD = log(DD$Absolute)
modelDD3<-lm(logtransDD~DISTANCE_KM,  data=DD)
Anova(modelDD3, type="II")
xDD3 = (residuals(modelDD3))
xDD3
#Cube root transformation
T_cubDD = sign(DD$Absolute) * abs(DD$Absolute)^(1/3)
modelDD4<-lm(T_cubDD~DISTANCE_KM,data=DD)
Anova(modelDD4, type="II")  ### THIS iS THE ONE 
xDD4 = (residuals(modelDD4))
xDD4
#Squaring transformation
squaringDD = (DD$Absolute)^2
modelDD5<-lm(squaringDD~DISTANCE_KM,  data=DD)
Anova(modelDD5, type="II")
xDD5 = (residuals(modelDD5))
xDD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDD)   #NT
shapiro.test(xDD2)  #Sqrt
shapiro.test(xDD3)  #Log
shapiro.test(xDD4)  #Cubert
shapiro.test(xDD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xDD)
plotNormalHistogram(xDD2)
plotNormalHistogram(xDD4)
plotNormalHistogram(xDD5)

qqnorm(residuals(modelDD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD),
       col="red")

qqnorm(residuals(modelDD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD2),
       col="red")

qqnorm(residuals(modelDD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD4),
       col="red")

qqnorm(residuals(modelDD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD5),
       col="red")


plot(fitted(modelDD),
     residuals(modelDD))

plot(fitted(modelDD2),
     residuals(modelDD2))

plot(fitted(modelDD4),
     residuals(modelDD4))

plot(fitted(modelDD5),
     residuals(modelDD5))


#Distance- Absolute weight relationship plotting
##regression
plot(DD$DISTANCE_KM,sqtransDD, main="Connection between distance among locations on C.serotinus Adult weight",
     ylab="Adult weight difference betweeen locations (DRT) (g)", xlab="Distance (km)")
abline(lm(sqtransDD~DD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransDD.lm = lm(sqtransDD~DD$DISTANCE_KM, data=DD)
summary(sqtransDD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Dianthus serotinus? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
DS$STZ=as.factor(DS$STZ)
modelDZ<- lme(root~STZ,random = ~ 1 | Location,data=DS)
modelDZ
Anova(modelDZ, type="II")
xDZ = (residuals(modelDZ))
xDZ
#Square root transformation
sqtransDZ = sqrt(DS$root)
modelDZ2<-lme(sqtransDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDZ2, type="II")
xDZ2 = (residuals(modelDZ2))
xDZ2
#Log transformation
logtransDZ = log(DS$root)
modelDZ3<-lme(logtransDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDZ3, type="II") ### THIS IS THE ONE
xDZ3 = (residuals(modelDZ3))
xDZ3
#Cube root transformation
T_cubDZ = sign(DS$root) * abs(DS$root)^(1/3)
modelDZ4<-lme(T_cubDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDZ4, type="II") 
xDZ4 = (residuals(modelDZ4))
xDZ4
#Squaring transformation
squaringDZ = (DS$root)^2
modelDS5<-lm(squaringDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDS5, type="II") 
xDZ5 = (residuals(modelDS5))
xDZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDZ)   #NT
shapiro.test(xDZ2)  #Sqrt
shapiro.test(xDZ3)  #Log
shapiro.test(xDZ4)  #Cubert
shapiro.test(xDZ5)  #Squaring
leveneTest(xDZ~STZ, DS) #NT
leveneTest(xDZ2~STZ, DS) #Sqrt
leveneTest(xDZ3~STZ, DS) #Log
leveneTest(xDZ4~STZ, DS) #Cubert
leveneTest(xDZ5~STZ, DS) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xDZ3)
skewness(xDZ3)
kurtosis(xDZ3)
qqnorm(residuals(modelDZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDZ3),
       col="red")
plot(fitted(modelDZ3),
     residuals(modelDZ))
TUKEYDZ<- summary(glht(modelDS5, linfct = mcp(STZ = "Tukey")), test = adjusted("holm"))
TUKEYDZ
# Assuming TUKEYDS is your summary object
significant_pairs <- sum(TUKEYDZ$test$pvalues < 0.05)
total_pairs <- length(TUKEYDZ$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")
#tDS<- glht(modelDS3, linfct = mcp(location= "Tukey"))
#t.cldDS <- cld(tDS)   # letter-based display
#t.cldDS

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Dianthus serotinus? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
modelDR <- lme(root~Aridity, random = ~ 1 | Location, data=DS)
summary(modelDR)
Anova(modelDR, type="II")
xDR = (residuals(modelDR))
xDR
#Square root transformation
sqtransDR = sqrt(DS$root)
modelDR2<-lme(sqtransDR~Aridity,random = ~ 1 | Location, data=DS)
Anova(modelDR2, type="II")
xDR2 = (residuals(modelDR2))
xDR2
#Log transformation
logtransDR = log(DS$root)
modelDR3<-lme(logtransDR~Aridity, random = ~ 1 | Location, data=DS)
Anova(modelDR3, type="II") ### THIS IS THE ONE
xDR3 = (residuals(modelDR3))
xDR3
#Cube root transformation
T_cubDR = sign(DS$root) * abs(DS$root)^(1/3)
modelDR4<-lme(T_cubDR~Aridity,random = ~ 1 | Location, data=DS)
Anova(modelDR4, type="II") 
xDR4 = (residuals(modelDR4))
xDR4
#Squaring transformation
squaringDR = (DS$root)^2
modelDR5<-lme(squaringDR~Aridity, random = ~ 1 | Location, data=DS)
Anova(modelDR5, type="II") 
xDR5 = (residuals(modelDR5))
xDR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDR)   #NT
shapiro.test(xDR2)  #Sqrt
shapiro.test(xDR3)  #Log
shapiro.test(xDR4)  #Cubert
shapiro.test(xDR5)  #Squaring
bartlett.test(DS$weight, DS$aridity)  #NT
bartlett.test(sqtransDR, DS$aridity)  #Sqrt
bartlett.test(logtransDR, DS$aridity) #Log
bartlett.test(T_cubDR, DS$aridity)    #Cubert
bartlett.test(squaringDR, DS$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xDR)
plotNormalHistogram(xDR2)
plotNormalHistogram(xDR3)
plotNormalHistogram(xDR4)
plotNormalHistogram(xDR5)
skewness(xDR3)
kurtosis(xDR3)
qqnorm(residuals(modelDR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDR3),
       col="red")
plot(fitted(modelDR3),
     residuals(modelDR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(DS, aes(x = Aridity, y = squaringDR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelDR5)))) 
#
######################################################
##########  LEAF   DRY  MATTER CONTENT ( L D M C)
###########
#####################################################

data<-read_xlsx("Adult_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
##Adult mass
###Location ~ LDMC
#No transformation
modelDS<- lm(LDMC~Location,data=DS)
modelDS
Anova(modelDS, type="II") 
xDS = (residuals(modelDS))
xDS
class(DS$LDMC)
DS$LDMC=as.numeric(DS$LDMC)
#Square root transformation
sqtransDS = sqrt(DS$LDMC)
modelDS2<-lm(sqtransDS~Location, data=DS)
Anova(modelDS2, type="II")
xDS2 = (residuals(modelDS2))
xDS2
#Log transformation
logtransDS = log(DS$LDMC)
modelDS3<-lm(logtransDS~Location, data=DS)
Anova(modelDS3, type="II") ##### THIS IS THE ONE
xDS3 = (residuals(modelDS3))
xDS3
#Cube root transformation
T_cubDS = sign(DS$LDMC) * abs(DS$LDMC)^(1/3)
modelDS4<-lm(T_cubDS~Location, data=DS)
Anova(modelDS4, type="II")
xDS4 = (residuals(modelDS4))
xDS4
#Squaring transformation
squaringDS = (DS$LDMC)^2
modelDS5<-lm(squaringDS~Location, data=DS)
###MODEL DS5
Anova(modelDS5, type="II") 
xDS5 = (residuals(modelDS5))
xDS5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDS)   #NT
shapiro.test(xDS2)  #Sqrt
shapiro.test(xDS3)  #Log
shapiro.test(xDS4)  #Cubert
shapiro.test(xDS5)  #Squaring

leveneTest(xDS~Location, DS) #NT
leveneTest(xDS2~Location, DS) #Sqrt
leveneTest(xDS3~Location, DS) #Log
leveneTest(xDS4~Location, DS) #Cubert
leveneTest(xDS5~Location, DS) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xDS)
plotNormalHistogram(xDS2)
plotNormalHistogram(xDS3)
plotNormalHistogram(xDS4)
plotNormalHistogram(xDS5)

qqnorm(residuals(modelDS),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS),
       col="red")

qqnorm(residuals(modelDS2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS2),
       col="red")

qqnorm(residuals(modelDS3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS3),
       col="red")

qqnorm(residuals(modelDS4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS4),
       col="red")

qqnorm(residuals(modelDS5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS5),
       col="red")

plot(fitted(modelDS),
     residuals(modelDS))

plot(fitted(modelDS2),
     residuals(modelDS2))

plot(fitted(modelDS3),
     residuals(modelDS3))

plot(fitted(modelDS4),
     residuals(modelDS4))


plot(fitted(modelDS5),
     residuals(modelDS5))
TUKEYDS<- summary(glht(modelDS3, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYDS
# Assuming TUKEYDS is your summary object
significant_pairs <- sum(TUKEYDS$test$pvalues < 0.05)
total_pairs <- length(TUKEYDS$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")


tDS<- glht(modelDS3, linfct = mcp(location= "Tukey"))
t.cldDS <- cld(tDS)   # letter-based display
t.cldDS

#What is the effect of Euclidean distance on Adult weight in Dianthus serotinus? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

DX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATADRAME
DY=DX%>% 
  filter(Species=="Dianthus serotinus")
DZ = DY %>%
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

write.csv(DZ, "DS_Adult_meanLDMC_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Dianthus serotinus? 
#Open file
DD<-read_xlsx("DS_Distance_LDMC.xlsx")
DD
#Best transformation
#No transformation
modelDD <- lm(Absolute~DISTANCE_KM , data=DD)
summary(modelDD)
Anova(modelDD, type="II")
xDD = (residuals(modelDD))
xDD
#Square root transformation
sqtransDD = sqrt(DD$Absolute)
modelDD2<-lm(sqtransDD~DISTANCE_KM, data=DD)
Anova(modelDD2, type="II") ### THIS iS THE ONE
xDD2 = (residuals(modelDD2))
xDD2
#Log transformation
logtransDD = log(DD$Absolute)
modelDD3<-lm(logtransDD~DISTANCE_KM,  data=DD)
Anova(modelDD3, type="II") 
xDD3 = (residuals(modelDD3))
xDD3
#Cube root transformation
T_cubDD = sign(DD$Absolute) * abs(DD$Absolute)^(1/3)
modelDD4<-lm(T_cubDD~DISTANCE_KM,data=DD)
Anova(modelDD4, type="II")    
xDD4 = (residuals(modelDD4))
xDD4
#Squaring transformation
squaringDD = (DD$Absolute)^2
modelDD5<-lm(squaringDD~DISTANCE_KM,  data=DD)
Anova(modelDD5, type="II")
xDD5 = (residuals(modelDD5))
xDD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDD)   #NT
shapiro.test(xDD2)  #Sqrt
shapiro.test(xDD3)  #Log
shapiro.test(xDD4)  #Cubert
shapiro.test(xDD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xDD)
plotNormalHistogram(xDD2)
plotNormalHistogram(xDD4)
plotNormalHistogram(xDD5)

qqnorm(residuals(modelDD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD),
       col="red")

qqnorm(residuals(modelDD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD2),
       col="red")

qqnorm(residuals(modelDD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD4),
       col="red")

qqnorm(residuals(modelDD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD5),
       col="red")


plot(fitted(modelDD),
     residuals(modelDD))

plot(fitted(modelDD2),
     residuals(modelDD2))

plot(fitted(modelDD4),
     residuals(modelDD4))

plot(fitted(modelDD5),
     residuals(modelDD5))


#Distance- Absolute weight relationship plotting
##regression
plot(DD$DISTANCE_KM,sqtransDD, main="Connection between distance among locations on C.serotinus Adult weight",
     ylab="Adult weight difference betweeen locations (DRT) (g)", xlab="Distance (km)")
abline(lm(sqtransDD~DD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransDD.lm = lm(sqtransDD~DD$DISTANCE_KM, data=DD)
summary(sqtransDD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Dianthus serotinus? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
DS$LDMC=as.numeric(DS$LDMC)
modelDZ <- lme(LDMC ~ STZ, random = ~ 1 | Location, data = DS, na.action=na.omit)
modelDZ
Anova(modelDZ, type="II")
xDZ = (residuals(modelDZ))
xDZ
#Square root transformation
sqtransDZ = sqrt(DS$LDMC)
modelDZ2<-lme(sqtransDZ~STZ,random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDZ2, type="II") 
xDZ2 = (residuals(modelDZ2))
xDZ2
#Log transformation
logtransDZ = log(DS$LDMC)
modelDZ3<-lme(logtransDZ~STZ,random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDZ3, type="II") ### THIS IS THE ONE
xDZ3 = (residuals(modelDZ3)) 
xDZ3
#Cube root transformation
T_cubDZ = sign(DS$LDMC) * abs(DS$LDMC)^(1/3)
modelDZ4<-lme(T_cubDZ~STZ,random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDZ4, type="II")
xDZ4 = (residuals(modelDZ4))
xDZ4
#Squaring transformation
squaringDZ = (DS$LDMC)^2
modelDS5<-lm(squaringDZ~STZ,random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDS5, type="II") 
xDZ5 = (residuals(modelDS5))
xDZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDZ)   #NT
shapiro.test(xDZ2)  #Sqrt
shapiro.test(xDZ3)  #Log
shapiro.test(xDZ4)  #Cubert
shapiro.test(xDZ5)  #Squaring
leveneTest(xDZ~STZ, DS) #NT
leveneTest(xDZ2~STZ, DS) #Sqrt
leveneTest(xDZ3~STZ, DS) #Log
leveneTest(xDZ4~STZ, DS) #Cubert
leveneTest(xDZ5~STZ, DS) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xDZ3)
skewness(xDZ3)
kurtosis(xDZ3)
qqnorm(residuals(modelDZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDZ3),
       col="red")
plot(fitted(modelDZ3),
     residuals(modelDZ))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Dianthus serotinus? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
DS$LDMC=as.numeric(DS$LDMC)
DS$Aridity=as.numeric(DS$Aridity)
modelDR <- lme(LDMC~Aridity, random = ~ 1 | Location, data=DS, na.action=na.omit)
summary(modelDR)
Anova(modelDR, type="II")
xDR = (residuals(modelDR))
xDR
#Square root transformation
sqtransDR = sqrt(DS$LDMC)
modelDR2<-lme(sqtransDR~Aridity,random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDR2, type="II") 
xDR2 = (residuals(modelDR2))
xDR2
#Log transformation
logtransDR = log(DS$LDMC)
modelDR3<-lme(logtransDR~Aridity, random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDR3, type="II")  ### THIS IS THE ONE
xDR3 = (residuals(modelDR3))
xDR3
#Cube root transformation
T_cubDR = sign(DS$LDMC) * abs(DS$LDMC)^(1/3)
modelDR4<-lme(T_cubDR~Aridity,random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDR4, type="II")
xDR4 = (residuals(modelDR4))
xDR4
#Squaring transformation
squaringDR = (DS$LDMC)^2
modelDR5<-lme(squaringDR~Aridity, random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDR5, type="II")
xDR5 = (residuals(modelDR5))
xDR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDR)   #NT
shapiro.test(xDR2)  #Sqrt
shapiro.test(xDR3)  #Log
shapiro.test(xDR4)  #Cubert
shapiro.test(xDR5)  #Squaring
bartlett.test(DS$weight, DS$aridity)  #NT
bartlett.test(sqtransDR, DS$aridity)  #Sqrt
bartlett.test(logtransDR, DS$aridity) #Log
bartlett.test(T_cubDR, DS$aridity)    #Cubert
bartlett.test(squaringDR, DS$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xDR3)
skewness(xDR3)
kurtosis(xDR3)
qqnorm(residuals(modelDR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDR3),
       col="red")
plot(fitted(modelDR3),
     residuals(modelDR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(DS, aes(x = Aridity, y = squaringDR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelDR5)))) 
#

######################################################
########## SPECIFIC LEAF AREA (S  L  A)
###########
#####################################################
data<-read_xlsx("Adult_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
##Adult mass
###Location ~ SLA
#No transformation
modelDS<- lm(SLA~Location,data=DS)
modelDS
Anova(modelDS, type="II") 
xDS = (residuals(modelDS))
xDS
class(DS$SLA)
DS$SLA=as.numeric(DS$SLA)
#Square root transformation
sqtransDS = sqrt(DS$SLA)
modelDS2<-lm(sqtransDS~Location, data=DS)
Anova(modelDS2, type="II")
xDS2 = (residuals(modelDS2))
xDS2
#Log transformation
logtransDS = log(DS$SLA)
modelDS3<-lm(logtransDS~Location, data=DS)
Anova(modelDS3, type="II") 
xDS3 = (residuals(modelDS3))
xDS3
#Cube root transformation
T_cubDS = sign(DS$SLA) * abs(DS$SLA)^(1/3)
modelDS4<-lm(T_cubDS~Location, data=DS)
Anova(modelDS4, type="II") ##### THIS IS THE ONE
xDS4 = (residuals(modelDS4))
xDS4
#Squaring transformation
squaringDS = (DS$SLA)^2
modelDS5<-lm(squaringDS~Location, data=DS)
#MODEL DS5
Anova(modelDS5, type="II") 
xDS5 = (residuals(modelDS5))
xDS5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDS)   #NT
shapiro.test(xDS2)  #Sqrt
shapiro.test(xDS3)  #Log
shapiro.test(xDS4)  #Cubert
shapiro.test(xDS5)  #Squaring

leveneTest(xDS~Location, DS) #NT
leveneTest(xDS2~Location, DS) #Sqrt
leveneTest(xDS3~Location, DS) #Log
leveneTest(xDS4~Location, DS) #Cubert
leveneTest(xDS5~Location, DS) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xDS)
plotNormalHistogram(xDS2)
plotNormalHistogram(xDS3)
plotNormalHistogram(xDS4)
plotNormalHistogram(xDS5)

qqnorm(residuals(modelDS),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS),
       col="red")

qqnorm(residuals(modelDS2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS2),
       col="red")

qqnorm(residuals(modelDS3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS3),
       col="red")

qqnorm(residuals(modelDS4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS4),
       col="red")

qqnorm(residuals(modelDS5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDS5),
       col="red")

plot(fitted(modelDS),
     residuals(modelDS))

plot(fitted(modelDS2),
     residuals(modelDS2))

plot(fitted(modelDS3),
     residuals(modelDS3))

plot(fitted(modelDS4),
     residuals(modelDS4))


plot(fitted(modelDS5),
     residuals(modelDS5))
TUKEYDS<- summary(glht(modelDS4, linfct = mcp(Location = "Tukey")), test = adjusted("holm"))
TUKEYDS
# Assuming TUKEYDS is your summary object
significant_pairs <- sum(TUKEYDS$test$pvalues < 0.05)
total_pairs <- length(TUKEYDS$test$pvalues)
# Display the count of significant pairs
cat("Number of significant pairs:", significant_pairs, "\n")
cat("Total number of pairs:", total_pairs, "\n")


tDS<- glht(modelDS3, linfct = mcp(location= "Tukey"))
t.cldDS <- cld(tDS)   # letter-based display
t.cldDS

#What is the effect of Euclidean distance on Adult weight in Dianthus serotinus? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

DX=read_xlsx("Adult_Masters.xlsx") #### OPEN OUR CLEANED DATADRAME
DY=DX%>% 
  filter(Species=="Dianthus serotinus")
DZ = DY %>%
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

#write.csv(DZ, "DS_Adult_meanSLA_root.csv")

#What is the effect of geographical distance (Km) on Adult weight in Dianthus serotinus? 
#Open file
DD<-read_xlsx("DS_Distance_SLA.xlsx")
DD
#Best transformation
#No transformation
modelDD <- lm(Absolute~DISTANCE_KM , data=DD)
summary(modelDD)
Anova(modelDD, type="II")
xDD = (residuals(modelDD))
xDD
#Square root transformation
sqtransDD = sqrt(DD$Absolute)
modelDD2<-lm(sqtransDD~DISTANCE_KM, data=DD)
Anova(modelDD2, type="II") 
xDD2 = (residuals(modelDD2))
xDD2
#Log transformation
logtransDD = log(DD$Absolute)
modelDD3<-lm(logtransDD~DISTANCE_KM,  data=DD)
Anova(modelDD3, type="II") ### THIS iS THE ONE
xDD3 = (residuals(modelDD3))
xDD3
#Cube root transformation
T_cubDD = sign(DD$Absolute) * abs(DD$Absolute)^(1/3)
modelDD4<-lm(T_cubDD~DISTANCE_KM,data=DD)
Anova(modelDD4, type="II")    
xDD4 = (residuals(modelDD4))
xDD4
#Squaring transformation
squaringDD = (DD$Absolute)^2
modelDD5<-lm(squaringDD~DISTANCE_KM,  data=DD)
Anova(modelDD5, type="II")
xDD5 = (residuals(modelDD5))
xDD5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDD)   #NT
shapiro.test(xDD2)  #Sqrt
shapiro.test(xDD3)  #Log
shapiro.test(xDD4)  #Cubert
shapiro.test(xDD5)  #Squaring

# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xDD)
plotNormalHistogram(xDD2)
plotNormalHistogram(xDD3)
plotNormalHistogram(xDD4)
plotNormalHistogram(xDD5)

qqnorm(residuals(modelDD),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD),
       col="red")

qqnorm(residuals(modelDD2),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD2),
       col="red")

qqnorm(residuals(modelDD4),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD4),
       col="red")

qqnorm(residuals(modelDD5),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDD5),
       col="red")


plot(fitted(modelDD),
     residuals(modelDD))

plot(fitted(modelDD2),
     residuals(modelDD2))

plot(fitted(modelDD4),
     residuals(modelDD4))

plot(fitted(modelDD5),
     residuals(modelDD5))


#Distance- Absolute weight relationship plotting
##regression
plot(DD$DISTANCE_KM,sqtransDD, main="Connection between distance among locations on C.serotinus Adult weight",
     ylab="Adult weight difference betweeen locations (DRT) (g)", xlab="Distance (km)")
abline(lm(sqtransDD~DD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransDD.lm = lm(sqtransDD~DD$DISTANCE_KM, data=DD)
summary(sqtransDD.lm) 

#What is the effect of Ecological distance (STZ) on Adult weight in Dianthus serotinus? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
DS$SLA=as.numeric(DS$SLA)
modelDZ <- lme(SLA ~ STZ, random = ~ 1 | Location, data = DS, na.action=na.omit)
modelDZ
Anova(modelDZ, type="II")
xDZ = (residuals(modelDZ))
xDZ
#Square root transformation
sqtransDZ = sqrt(DS$SLA)
modelDZ2<-lme(sqtransDZ~STZ,random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDZ2, type="II") 
xDZ2 = (residuals(modelDZ2))
xDZ2
#Log transformation
logtransDZ = log(DS$SLA)
modelDZ3<-lme(logtransDZ~STZ,random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDZ3, type="II") ### THIS IS THE ONE
xDZ3 = (residuals(modelDZ3)) 
xDZ3
#Cube root transformation
T_cubDZ = sign(DS$SLA) * abs(DS$SLA)^(1/3)
modelDZ4<-lme(T_cubDZ~STZ,random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDZ4, type="II")
xDZ4 = (residuals(modelDZ4))
xDZ4
#Squaring transformation
squaringDZ = (DS$SLA)^2
modelDS5<-lm(squaringDZ~STZ,random = ~ 1 | Location, data=DS, na.action=na.omit)
###MODEL DS5
Anova(modelDS5, type="II") 
xDZ5 = (residuals(modelDS5))
xDZ5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDZ)   #NT
shapiro.test(xDZ2)  #Sqrt
shapiro.test(xDZ3)  #Log
shapiro.test(xDZ4)  #Cubert
shapiro.test(xDZ5)  #Squaring
leveneTest(xDZ~STZ, DS) #NT
leveneTest(xDZ2~STZ, DS) #Sqrt
leveneTest(xDZ3~STZ, DS) #Log
leveneTest(xDZ4~STZ, DS) #Cubert
leveneTest(xDZ5~STZ, DS) #Squaring
# Best transformation: Log
#Plotting residuals with best trasnformation
plotNormalHistogram(xDZ)
plotNormalHistogram(xDZ2)
plotNormalHistogram(xDZ3)
plotNormalHistogram(xDZ4)
plotNormalHistogram(xDZ5)




skewness(xDZ3)
kurtosis(xDZ3)
qqnorm(residuals(modelDZ3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDZ3),
       col="red")
plot(fitted(modelDZ3),
     residuals(modelDZ))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What is the effect of Environmental distance (Aridity) on Adult weight in Dianthus serotinus? 
#Best transformation
#No transformation
data<-read_xlsx("Adult_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
DS$SLA=as.numeric(DS$SLA)
DS$Aridity=as.numeric(DS$Aridity)
modelDR <- lme(SLA~Aridity, random = ~ 1 | Location, data=DS, na.action=na.omit)
summary(modelDR)
Anova(modelDR, type="II")
xDR = (residuals(modelDR))
xDR
#Square root transformation
sqtransDR = sqrt(DS$SLA)
modelDR2<-lme(sqtransDR~Aridity,random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDR2, type="II") 
xDR2 = (residuals(modelDR2))
xDR2
#Log transformation
logtransDR = log(DS$SLA)
modelDR3<-lme(logtransDR~Aridity, random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDR3, type="II")  ### THIS IS THE ONE
xDR3 = (residuals(modelDR3))
xDR3
#Cube root transformation
T_cubDR = sign(DS$SLA) * abs(DS$SLA)^(1/3)
modelDR4<-lme(T_cubDR~Aridity,random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDR4, type="II")
xDR4 = (residuals(modelDR4))
xDR4
#Squaring transformation
squaringDR = (DS$SLA)^2
modelDR5<-lme(squaringDR~Aridity, random = ~ 1 | Location, data=DS, na.action=na.omit)
Anova(modelDR5, type="II")
xDR5 = (residuals(modelDR5))
xDR5
# Shapiro (Normality) and bartlett (homoscedasticity) tests
shapiro.test(xDR)   #NT
shapiro.test(xDR2)  #Sqrt
shapiro.test(xDR3)  #Log
shapiro.test(xDR4)  #Cubert
shapiro.test(xDR5)  #Squaring
bartlett.test(DS$weight, DS$aridity)  #NT
bartlett.test(sqtransDR, DS$aridity)  #Sqrt
bartlett.test(logtransDR, DS$aridity) #Log
bartlett.test(T_cubDR, DS$aridity)    #Cubert
bartlett.test(squaringDR, DS$aridity) #Squaring

#Plotting residuals with best trasnformation
plotNormalHistogram(xDR3)
skewness(xDR3)
kurtosis(xDR3)
qqnorm(residuals(modelDR3),
       ylab="Sample Quantiles for residuals")
qqline(residuals(modelDR3),
       col="red")
plot(fitted(modelDR3),
     residuals(modelDR3))
# Best transformation: Log
#Aridity-weight relationship plotting
ggplot(DS, aes(x = Aridity, y = squaringDR, colour=STZ)) +
  labs(x="Aridity index",y="Adult weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelDR5)))) 
#











