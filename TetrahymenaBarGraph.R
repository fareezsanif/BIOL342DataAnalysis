#Tetra Growth Curve Bar Graph

#Set and format Data 
rawData <- read.csv("growthRate.csv")

TControl <- rawData[1:5,2]
T5 <- rawData[6:10,2]
T50 <- rawData[11:15,2]
T500 <- rawData[16:20,2]

t <- c(0,0,0,0,0, 5,5,5,5,5, 50,50,50,50,50, 500,500,500,500,500)
catData <- data.frame(rate = c(TControl, T5, T50, T500), treatment = t )
catData

str(catData)
summary(catData)




library(FSA)
Sum = Summarize(rate ~ treatment,
                    data   = catData,
                    conf   = 0.95,
                    digits = 3)
Sum$se = Sum$sd / sqrt(Sum$n)

library(ggplot2)
library(ggpubr)


ggerrorplot(catData, x = "treatment", y = "rate", add = c("mean_se", "jitter"), 
		combine = TRUE, color = "treatment", palette = c("#FF8C00", "#006400", "#008080", "#4B0082"), ylab = "Growth Rate (Per Hour)",
		 xlab = "Concentration of Ammonium Ions in Culture (mg/L)", ylim = c(0,13))


#Strip Chart
#http://www.sthda.com/english/wiki/ggplot2-stripchart-jitter-quick-start-guide-r-software-and-data-visualization
library(ggplot2)
p <- ggplot(catData, aes(x=treatment, y=rate)) + 
    geom_jitter(position=position_jitter(0.2))
p
p + stat_summary(fun.data=mean_sdl, mult=1, 
                 geom="pointrange", color="red")	


