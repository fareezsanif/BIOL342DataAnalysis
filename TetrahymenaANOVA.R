#BIOL 342 Tetra ANOVA


#Set and format Data correctly for ANOVA
rawData <- read.csv("growthRate.csv")

TControl <- rawData[1:5,2]
T5 <- rawData[6:10,2]
T50 <- rawData[11:15,2]
T500 <- rawData[16:20,2]

#QQplots for normality
qqnorm(TControl, pch = 1, frame=FALSE)
qqline(TControl, col = "steelblue", lwd = 2)

qqnorm(T5, pch = 1, frame=FALSE)
qqline(T5, col = "steelblue", lwd = 2)

qqnorm(T50, pch = 1, frame=FALSE)
qqline(T50, col = "steelblue", lwd = 2)

qqnorm(T500, pch = 1, frame=FALSE)
qqline(T500, col = "steelblue", lwd = 2)


t <- c(0,0,0,0,0, 5,5,5,5,5, 50,50,50,50,50, 500,500,500,500,500)
anovaData <- data.frame(rate = c(TControl, T5, T50, T500), treatment = t )
anovaData

levels(anovaData$treatment)


#ANOVA Time

#Generate Summary
library(dplyr)

group_by(anovaData, treatment) %>%
  summarise(
    count = n(),
    mean = mean(rate, na.rm = TRUE),
    sd = sd(rate, na.rm = TRUE))


#Boxplot
library("ggpubr")
ggboxplot(anovaData, x = "treatment", y = "rate", 
          order = c(0, 5, 50, 500),
          ylab = "Growth Rate (Per Hour)", xlab = "Concentration of Ammonium Ions (mg/L)")

#Actual ANOVA
res.aov <- aov(rate ~ treatment, data = anovaData)
summary(res.aov)

#Kruskal Wallace
k.aov <- kruskal.test(rate ~ treatment, data = anovaData)
k.aov


#Post Hoc
library(DescTools)

PostHocTest(k.aov, method = "newmankeuls")

