#BIOL 342 Tetra Growth Rate Calculation

rawData <- read.csv("TTCollectedData.csv")

#http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/


logData <- data.frame("Time" = as.numeric(rawData[1,3:8]))

i <- 1
seq <- c(3,4,5,6,7,9,10,11,12,13,15,16,17,18,19,21,22,23,24,25)
for(val in seq) {
	logData <- cbind(logData, data = log(as.numeric(rawData[val,3:8])))
	i <- i + 1
}
logData

growthRates <- c()
i <- 2
while (i <= 21) {
	model <- lm(logData[,1] ~ logData[,i])
	growthRates <- c(growthRates, as.numeric(coef(model)[2]))
	i <- i + 1
}
growthRates

summary(growthRates[1:5])
summary(growthRates[6:10])
summary(growthRates[11:15])
summary(growthRates[16:20])

write.csv(growthRates, "growthRate.csv")

