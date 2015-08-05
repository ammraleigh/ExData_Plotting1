plot4 <- function () {
  
  ## Read in the dataset
  myData <- read.table("household_power_consumption.txt", sep = ";", na.strings = "?", header = TRUE)
  
  ## Convert the factor types to character strings
  myData <- as.data.frame(lapply(myData,function (y) if(class(y)=="factor" ) as.character(y) else y),stringsAsFactors=F)
  
  ## Convert the character string Date to Date object
  myData$Date <- as.Date(substr(myData$Date,1,10), "%d/%m/%Y")
  
  ## Filter on the date range
  myDates <- subset(myData, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
  
  ## Create the Date/Time objectt
  myDates$DateTime <- paste(myDates$Date, myDates$Time)
  myDates$DateTime2 <-strptime(myDates$DateTime, "%Y-%m-%d %H:%M:%S")
  
  ## Create the plot and save it as PNG
  png(filename = "plot4.png", width=480, height=480, units = "px")
  
  par(mfcol=(c(2,2)))
  
  plot(myDates$DateTime2, myDates$Global_active_power, type="l", pch="|", xlab = "", ylab = "Global Active Power")
  
  plot(c(min(myDates$DateTime2),max(myDates$DateTime2)),c(min(myDates$Sub_metering_1),max(myDates$Sub_metering_1)),type="n", xlab = "", ylab = "Energy sub metering")
  lines(myDates$DateTime2, myDates$Sub_metering_1, col = "black", lwd=1)
  lines(myDates$DateTime2, myDates$Sub_metering_2, col = "red", lwd=1)
  lines(myDates$DateTime2, myDates$Sub_metering_3, col = "blue",  lwd=1)
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty= "n",  lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("black", "red","blue"))

  plot(myDates$DateTime2, myDates$Voltage, type="l", pch="|", xlab = "datetime", ylab = "Voltage", lwd=.5)

  plot(myDates$DateTime2, myDates$Global_reactive_power, type="l", pch="|", xlab = "datetime", lwd=.5, ylab = "Global_reactive_power")

  dev.off()
  
}
