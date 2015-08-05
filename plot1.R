plot1 <- function () {
  
  myData <- read.table("household_power_consumption.txt", sep = ";", na.strings = "?", header = TRUE)
  myData <- as.data.frame(lapply(myData,function (y) if(class(y)=="factor" ) as.character(y) else y),stringsAsFactors=F)
  myData$Date <- as.Date(substr(myData$Date,1,10), "%d/%m/%Y")
  myDates <- subset(myData, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
  
  png(filename = "plot1.png", width=480, height=480, units = "px")
  hist(myDates$Global_active_power, main = "Global Active Power", col = "red", xlab = "Global Active Power (kilowatts)")
  dev.off()
  
}
