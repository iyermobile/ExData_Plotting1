## Assignment - Coursera : Exploratory Data Analysis
## By iyermobile | https://github.com/iyermobile/ExData_Plotting1
## JAN-12-2015

DownloadFile <- function(fileURL, myfilename) {
    if(!file.exists(myfilename)) {
        download.file(fileURL, destfile=myfilename, method="curl")
    }
    myfilename
}

# First read the ZIP format data file
myData <- function() {
    cacheFile <- "myplot_data.csv"
    if(file.exists(cacheFile)) {
        myTable <- read.csv(cacheFile)
        myTable$DateTime <- strptime(myTable$DateTime, "%Y-%m-%d %H:%M:%S")
    }
    else {
        myfilename <- DownloadFile("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", "exdata-data-household_power_consumption.zip")

	# Create the text connection and load data
		myConn <- unz(myfilename, "household_power_consumption.txt")
		myTable <- read.table(myConn, header=T, sep=';', na.strings="?", colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
		#close(myConn)

	# Check the dates and read lines if they match
		myTable <- myTable[(myTable$Date == "1/2/2007") | (myTable$Date == "2/2/2007"),]
		myTable$DateTime <- strptime(paste(myTable$Date, myTable$Time), "%d/%m/%Y %H:%M:%S")
		write.csv(myTable, cacheFile)
    }
    myTable
}

# Save the "Energy sub metering" plot output to png with default white background using size 480 x 480 as specified in the rubric.

myPlot3 <- function() {
    myTable <- myData()
    png(filename = "plot3.png", width = 480, height = 480, units = "px")
    cols = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    plot(myTable$DateTime, myTable$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
    lines(myTable$DateTime, myTable$Sub_metering_2, type="l", col="red")
    lines(myTable$DateTime, myTable$Sub_metering_3, type="l", col="blue")
    legend("topright", lty=1, lwd=1, col=c("black","blue","red"), legend=cols)
    dev.off()
}

myPlot3()
