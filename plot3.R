#######################################################################################################################
## Function makePlot3 performs the following 
##  - Calls the function to download and unzip the data
##  - Calls the function to read the data 
##  - Creates a subset of data for Baltimore, Maryland
##  - Activates the PNG output device and generates the plot
##  - Closes the PNG output device
## Parameters: wd = the working directory
##             pPNG = generate the PNG output
#######################################################################################################################

makePlot3 <- function(wd, pPNG = TRUE) {
  
  library(ggplot2)
  
	if (!getwd() == wd) {
		setwd(wd)
	}
	
  ##Check if data is supplied, otherwise get it
  if (is.null(NEI)) {
    fetchSourceData()
  	NEI <- readSourceData()
  }
  ##Initialize the PNG device
  if (pPNG == TRUE) {
    png(filename = "plot3.png",
        width = 1920, height = 480, 
        units = "px", pointsize = 12,
        bg = "white")
  }
  ##Create a subset for Baltimore City, Maryland
  NEI_BM <- NEI[which(NEI$fips == "24510"), ]
  ##Summarize the PM2.5 emissions by year and type
  tbyt_BM <- aggregate(NEI_BM$Emissions, by=list(Year=NEI_BM$year, Type=NEI_BM$type), FUN=sum, na.rm=TRUE)
  ##Now plot the results
  qp = qplot(y=x, x=Year, data=tbyt_BM, facets=.~Type, 
  					 geom=c("point", "smooth"), method="lm", 
  					 ylab="PM2.5 Emissions (tons)",
  					 main="Total Emissions for Baltimore, by Type") +
  	coord_cartesian(ylim = c(0, 3000))
  print(qp)
  ##Close the PNG device
  if (pPNG == TRUE) {
    dev.off()  
  }
}