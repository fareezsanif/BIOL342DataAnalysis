#BIOL 342 Tetrahymena Growth Curves

#O2 is the oxygen concentration in the water at location in mg/L
times <- c(0, 2, 4, 6, 8)
countc <- c(1.00E+04,	2.70E+03,	5.04E+03,	1.07E+04,	1.20E+04)
countt1 <- c(1.99E+03,	6.45E+03,	4.69E+03,	6.45E+03,	7.38E+03)
countt2 <- c(6.80E+03,	7.62E+03,	1.35E+04,	2.09E+04,	2.31E+04)
countt3 <- c(6.80E+03,	6.56E+03,	4.45E+03,	1.18E+04,	1.35E+04)

times4 <- c(times, times, times, times)
times4

counts <- c(log(countc), log(countt1), log(countt2), log(countt3))
counts

#Actual Counts

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

realTimes <- c(0,2,4,6,8,24)
realTimes4 <- c(realTimes, realTimes, realTimes, realTimes)




logCounts <- c(rowMeans(cbind(logData[0,2],logData[,3],logData[,4],logData[,5], logData[0,6])), +
			rowMeans(cbind(logData[,7],logData[,8],logData[,9],logData[0,10],logData[0,11])), +
			rowMeans(cbind(logData[0,12],logData[,13],logData[,14],logData[,15],logData[0,16])), + 
			rowMeans(cbind(logData[0,17],logData[,18],logData[,19],logData[,20],logData[0,21])))

#category
cat <- c(0,0,0,0,0,0)
y <- 0
x <- 5
while (y < 3){
cat <- c(cat,x * 10^y,x * 10^y,x * 10^y,x * 10^y,x * 10^y, x * 10^y)
y <- y + 1}


# create dummy data 
#TODO Mess with the Data frame so I can add them to the same plot
d <- data.frame(
  t = times4,
  c = counts,
  cyl = cat
)


#Create Real Dummy Data
d2 <- data.frame(
  t = realTimes4,
  c = logCounts,
  cyl = cat
)


# Load easyGgplot2
library(easyGgplot2)


#Build Scatter Plot
p <- ggplot2.scatterplot(data=d2, xName = 't', yName = 'c', 
			groupName = 'cyl', backgroundColor= 'white',
      		addRegLine=TRUE, axisLine=c(1, "solid", "darkblue"), regLineSize = 1,
			addConfidenceInterval = FALSE,
			#Title Information
			 			xtitle="Time (hrs)", ytitle="log10(Cell Count)",
			
			#Legend Information
			legendPosition="right",

 			legendTitle="NH4+ Treatment (mg/L)", legendTitleFont=c(10, "bold", "blue"),

  		  	legendTextFont=c(10, "bold.italic", "black"),

    			legendBackground=c("lightblue", 0.5, "solid", "darkblue" ))
p

#Graph that sucks
# create dummy data 
sad <- c(countc, countt1, countt2, countt3)


#TODO Mess with the Data frame so I can add them to the same plot
sd <- data.frame(
  t = times4,
  c = sad,
  cyl = cat
)

#Build Sad Scatter Plot
s <- ggplot2.scatterplot(data=sd, xName = 't', yName = 'c', 
			groupName = 'cyl', backgroundColor= 'white',
      		addRegLine=TRUE, axisLine=c(1, "solid", "darkblue"), regLineSize = 1,
			addConfidenceInterval = TRUE, smoothingMethod="loess",
			#Title Information
			mainTitle="Cell count of Tetrahymena vs Time",

     			xtitle="Time (hrs)", ytitle="Cell Count",
			
			#Legend Information
			legendPosition="right",

 			legendTitle="NH4+ Treatment (mg/L)", legendTitleFont=c(10, "bold", "blue"),

  		  	legendTextFont=c(10, "bold.italic", "black"),

    			legendBackground=c("lightblue", 0.5, "solid", "darkblue" ))
s



