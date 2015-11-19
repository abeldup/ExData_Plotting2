#######################################################################################################################
## Function makePlot2 performs the following 
##  - Calls the function to download and unzip the data
##  - Calls the function to read the data 
##  - Creates a subset of data for Baltimore, Maryland
##  - Activates the PNG output device and generates the plot
##  - Closes the PNG output device
## Parameters: pPNG = generate the PNG output
#######################################################################################################################

makePlot2 <- function(pPNG = TRUE) {
  
  library(graphics)
  library(grDevices)
  
  setwd("~/Documents/GitHub/datasciencecoursera/ExData_Plotting2")
  
  ##Check if data is supplied, otherwise get it
  if (is.null(NEI)) {
    fetchSourceData()
    readSourceData()
  }
  ##Initialize the PNG device
  if (pPNG == TRUE) {
    png(filename = "plot2.png",
        width = 480, height = 480, 
        units = "px", pointsize = 12,
        bg = "white")
  }
  ##Create a subset for Baltimore City, Maryland
  if (is.null(NEI_BM)) {
    NEI_BM <- NEI[which(NEI$fips == "24510"), ]
  }
  ##Summarize the PM2.5 emissions by year
  if (is.null(tby_BM)) {
    tby_BM <- aggregate(NEI_BM$Emissions, by=list(Year=NEI_BM$year), FUN=sum, na.rm=TRUE)
  }
  ##Calculate a smooting spline
  SS2 <- smooth.spline(tby_BM$Year, tby_BM$x, spar=0.35)
  ##Now plot the results
  plot(tby_BM, type="l", col="Blue", main="Total Emissions for Baltimore, Maryland", 
       ylab="PM2.5 Emissions (tons)", xlab="Year", xlim=c(1998, 2010))
  points(tby_BM, pch=10)
  lines(SS2, col="Red", lty="dotdash")
  legend("topright", c("PM2.5","Trend"), lty=c(1,5), col=c("Blue","Red"))
  ##Close the PNG device
  if (pPNG == TRUE) {
    dev.off()  
  }
}