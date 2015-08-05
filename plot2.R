plot2 <- function () {
  
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
  png(filename = "plot2.png", width=480, height=480, units = "px")
  plot(myDates$DateTime2, myDates$Global_active_power, type="l", pch="|", xlab = "", ylab = "Global Active Power (kilowatts)")
  dev.off()
  
}
