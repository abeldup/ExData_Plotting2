#######################################################################################################################
## Function makePlot1 performs the following 
##  - Calls the function to download and unzip the data
##  - Calls the function to read the data 
##  - Activates the PNG output device and generates the plot
##  - Closes the PNG output device
## Parameters: pwd = the working directory
##             pPNG = generate the PNG output
#######################################################################################################################

makePlot1 <- function(pwd, pPNG = TRUE) {
  
  library(graphics)
  library(grDevices)
  
	if (!getwd() == pwd) {
  	setwd(pwd)
	}
	
  ##Check if data is supplied, otherwise get it
  if (is.null(NEI)) {
    fetchSourceData()
    NEI <- readSourceData()
  }
  ##Initialize the PNG device
  if (pPNG == TRUE) {
    png(filename = "plot1.png",
        width = 480, height = 480, 
        units = "px", pointsize = 12,
        bg = "white")
  }
  ##Summarize the PM2.5 emissions by year
  tby <- aggregate(NEI$Emissions, by=list(Year=NEI$year), FUN=sum, na.rm=TRUE)
  ##Calculate a smooting spline
  SS1 <- smooth.spline(tby$Year, tby$x, spar=0.35)
  ##Now plot the results
  plot(tby, type="l", col="Blue", main="Total Emissions", 
       ylab="PM2.5 Emissions (tons)", xlab="Year", xlim=c(1998, 2010))
  points(tby, pch=10)
  lines(SS1, col="Red", lty="dotdash")
  legend("topright", c("PM2.5","Trend"), lty=c(1,5), col=c("Blue","Red"))
  ##Close the PNG device
  if (pPNG == TRUE) {
    dev.off()  
  }
  return(NEI)
}