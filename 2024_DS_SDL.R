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
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
##Seedling mass
###Location ~ SSM
#No transformation
modelDS<- lm(SSM~Location,data=DS)
modelDS
Anova(modelDS, type="II")
xDS = (residuals(modelDS))
xDS
#Square root transformation
sqtransDS = sqrt(DS$SSM)
modelDS2<-lm(sqtransDS~Location, data=DS)
Anova(modelDS2, type="II")
xDS2 = (residuals(modelDS2))
xDS2
#Log transformation
logtransDS = log(DS$SSM)
modelDS3<-lm(logtransDS~Location, data=DS)
Anova(modelDS3, type="II") ##### THIS IS THE ONE
xDS3 = (residuals(modelDS3))
xDS3

#Cube root transformation
T_cubDS = sign(DS$SSM) * abs(DS$SSM)^(1/3)
modelDS4<-lm(T_cubDS~Location, data=DS)
Anova(modelDS4, type="II")
xDS4 = (residuals(modelDS4))
xDS4
#Squaring transformation
squaringDS = (DS$SSM)^2
modelDS5<-lm(squaringDS~Location, data=DS)
####CORRECTED MODEL DS5
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

#What is the effect of geographical distance on seedling weight in Dianthus serotinus? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

DX=read_xlsx("Seedling_Masters.xlsx") #### OPEN OUR CLEANED DATAFRAME
DY=DX%>% 
  filter(Species=="Dianthus serotinus")
DZ=DY%>% 
  group_by(Location)%>% 
  summarize(
    avg_SSM=mean(SSM),
    avg_SSM=mean(SSM))

#write.csv(DZ, "DianthusmeanSSM_SSM.csv")

#What is the effect of geographical distance (Km) on seedling weight in Dianthus serotinus? 
#Open file
DD<-read_xlsx("DS_Distance_SSM.xlsx")
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
###CORRECTED MODEL DD5
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
plot(DD$DISTANCE_KM,sqtransDD, main="Connection between distance among locations on C.arenaria Seedling weight",
     ylab="Seedling weight difference betweeen locations (DRT) (g)", xlab="Distance (km)")
abline(lm(sqtransDD~DD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransDD.lm = lm(sqtransDD~DD$DISTANCE_KM, data=DD)
summary(sqtransDD.lm) 

#What is the effect of Ecological distance (STZ) on seedling weight in Dianthus serotinus? 
#Best transformation
#No transformation
data<-read_xlsx("Seedling_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)

modelDZ<- lme(SSM~STZ,random = ~ 1 | Location,data=DS)
modelDZ
Anova(modelDZ, type="II")
xDZ = (residuals(modelDZ))
xDZ
#Square root transformation
sqtransDZ = sqrt(DS$SSM)
modelDZ2<-lme(sqtransDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDZ2, type="II")
xDZ2 = (residuals(modelDZ2))
xDZ2
#Log transformation
logtransDZ = log(DS$SSM)
modelDZ3<-lme(logtransDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDZ3, type="II") ### THIS IS THE ONE
xDZ3 = (residuals(modelDZ3)) 
xDZ3
#Cube root transformation
T_cubDZ = sign(DS$SSM) * abs(DS$SSM)^(1/3)
modelDZ4<-lme(T_cubDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDZ4, type="II") 
xDZ4 = (residuals(modelDZ4))
xDZ4
#Squaring transformation
squaringDZ = (DS$SSM)^2
modelDS5<-lm(squaringDZ~STZ,random = ~ 1 | Location, data=DS)
###CORRECTED MODEL DS5
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
#What is the effect of Environmental distance (Aridity) on seedling weight in Dianthus serotinus? 
#Best transformation
#No transformation
data<-read_xlsx("Seedling_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
modelDR <- lme(SSM~Aridity, random = ~ 1 | Location, data=DS)
summary(modelDR)
Anova(modelDR, type="II")
xDR = (residuals(modelDR))
xDR
#Square root transformation
sqtransDR = sqrt(DS$SSM)
modelDR2<-lme(sqtransDR~Aridity,random = ~ 1 | Location, data=DS)
Anova(modelDR2, type="II")
xDR2 = (residuals(modelDR2))
xDR2
#Log transformation
logtransDR = log(DS$SSM)
modelDR3<-lme(logtransDR~Aridity, random = ~ 1 | Location, data=DS)
Anova(modelDR3, type="II") ### THIS IS THE ONE
xDR3 = (residuals(modelDR3))
xDR3
#Cube root transformation
T_cubDR = sign(DS$weight) * abs(DS$weight)^(1/3)
modelDR4<-lme(T_cubDR~aridity,random = ~ 1 | location, data=DS)
Anova(modelDR4, type="II")
xDR4 = (residuals(modelDR4))
xDR4
#Squaring transformation
squaringDR = (DS$weight)^2
modelDR5<-lme(squaringDR~aridity, random = ~ 1 | location, data=DS)
##CORRECTED MODELDR5
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
ggplot(DS, aes(x = Aridity, y = logtransDR, colour=STZ)) +
  labs(x="Aridity index",y="Seedling weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelDR3)))) 
#

######################################################
##########  S E E D L I N G      H E I G H T 
###########
#####################################################
data<-read_xlsx("Seedling_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
##Seedling mass
###Location ~ SLH
#No transformation
modelDS<- lm(SLH~Location,data=DS)
modelDS
Anova(modelDS, type="II")
xDS = (residuals(modelDS))
xDS
#Square root transformation
sqtransDS = sqrt(DS$SLH)
modelDS2<-lm(sqtransDS~Location, data=DS)
Anova(modelDS2, type="II")
xDS2 = (residuals(modelDS2))
xDS2
#Log transformation
logtransDS = log(DS$SLH)
modelDS3<-lm(logtransDS~Location, data=DS)
Anova(modelDS3, type="II") 
xDS3 = (residuals(modelDS3))
xDS3

#Cube root transformation
T_cubDS = sign(DS$SLH) * abs(DS$SLH)^(1/3)
modelDS4<-lm(T_cubDS~Location, data=DS)
Anova(modelDS4, type="II") ##### THIS IS THE ONE
xDS4 = (residuals(modelDS4))
xDS4
#Squaring transformation
squaringDS = (DS$SLH)^2
modelDS5<-lm(squaringDS~Location, data=DS)
###CORRECTED MODEL DS5
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

#What is the effect of GEOGRAPHICAL distance on seedling weight in Dianthus serotinus? 
#Best transformation
# for distance fisrtly we calcuated the mean by location and the make the table for calculation

DX=read_xlsx("Seedling_Masters.xlsx") #### OPEN OUR CLEANED DATAFRAME
DY=DX%>% 
  filter(Species=="Dianthus serotinus")
DZ=DY%>% 
  group_by(Location)%>% 
  summarize(
    avg_SLH=mean(SLH),
    avg_SLH=mean(SLH))

#write.csv(DZ, "DianthusmeanSLH_SLH.csv")

#What is the effect of geographical distance (Km) on seedling weight in Dianthus serotinus? 
#Open file
DD<-read_xlsx("DS_Distance_SLH.xlsx")
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
###CORRECTED MODEL DD5
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
plot(DD$DISTANCE_KM,sqtransDD, main="Connection between distance among locations on C.arenaria Seedling weight",
     ylab="Seedling weight difference betweeen locations (DRT) (g)", xlab="Distance (km)")
abline(lm(sqtransDD~DD$DISTANCE_KM), col="red") # regression line (y~x)
sqtransDD.lm = lm(sqtransDD~DD$DISTANCE_KM, data=DD)
summary(sqtransDD.lm) 

#What is the effect of Ecological distance (STZ) on seedling weight in Dianthus serotinus? 
#Best transformation
#No transformation
data<-read_xlsx("Seedling_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)

modelDZ<- lme(SLH~STZ,random = ~ 1 | Location,data=DS)
modelDZ
Anova(modelDZ, type="II")
xDZ = (residuals(modelDZ))
xDZ
#Square root transformation
sqtransDZ = sqrt(DS$SLH)
modelDZ2<-lme(sqtransDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDZ2, type="II")
xDZ2 = (residuals(modelDZ2))
xDZ2
#Log transformation
logtransDZ = log(DS$SLH)
modelDZ3<-lme(logtransDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDZ3, type="II") 
xDZ3 = (residuals(modelDZ3)) 
xDZ3
#Cube root transformation
T_cubDZ = sign(DS$SLH) * abs(DS$SLH)^(1/3)
modelDZ4<-lme(T_cubDZ~STZ,random = ~ 1 | Location, data=DS)
Anova(modelDZ4, type="II") ### THIS IS THE ONE
xDZ4 = (residuals(modelDZ4))
xDZ4
#Squaring transformation
squaringDZ = (DS$SLH)^2
modelDS5<-lm(squaringDZ~STZ,random = ~ 1 | Location, data=DS)
####CORRECTED MODEL DS5
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
#What is the effect of Environmental distance (Aridity) on seedling weight in Dianthus serotinus? 
#Best transformation
#No transformation
data<-read_xlsx("Seedling_Masters.xlsx")
DS=data%>% 
  filter(Species=="Dianthus serotinus")
DS$Location=as.factor(DS$Location)
modelDR <- lme(SLH~Aridity, random = ~ 1 | Location, data=DS)
summary(modelDR)
Anova(modelDR, type="II")
xDR = (residuals(modelDR))
xDR
#Square root transformation
sqtransDR = sqrt(DS$SLH)
modelDR2<-lme(sqtransDR~Aridity,random = ~ 1 | Location, data=DS)
Anova(modelDR2, type="II")
xDR2 = (residuals(modelDR2))
xDR2
#Log transformation
logtransDR = log(DS$SLH)
modelDR3<-lme(logtransDR~Aridity, random = ~ 1 | Location, data=DS)
Anova(modelDR3, type="II") 
xDR3 = (residuals(modelDR3))
xDR3
#Cube root transformation
T_cubDR = sign(DS$SLH) * abs(DS$SLH)^(1/3)
modelDR4<-lme(T_cubDR~Aridity,random = ~ 1 | Location, data=DS)
Anova(modelDR4, type="II") ### THIS IS THE ONE
xDR4 = (residuals(modelDR4))
xDR4
#Squaring transformation
squaringDR = (DS$SLH)^2
modelDR5<-lme(squaringDR~Aridity, random = ~ 1 | Location, data=DS)
###CORRECTED MODEL DR5
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
ggplot(DS, aes(x = Aridity, y = T_cubDR, colour=STZ)) +
  labs(x="Aridity index",y="Seedling weight (g)")+
  geom_point(shape = 16, size=1.8) +
  geom_abline(aes(intercept=`(Intercept)`, slope=Aridity), as.data.frame(t(fixef(modelDR4)))) 
#


