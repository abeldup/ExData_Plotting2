#######################################################################################################################
## Function makePlot3 performs the following 
##  - Calls the function to download and unzip the data
##  - Calls the function to read the data 
##  - Creates a subset of data for Baltimore, Maryland
##  - Summarizes the subset by year and type
##  - Activates the PNG output device and generates the plot
##  - Closes the PNG output device
## Parameters: pwd = the working directory
##             pPNG = generate the PNG output
## Returns: NEI (so that it can be re-used)
#######################################################################################################################

makePlot3 <- function(pwd, pPNG = TRUE) {
  
  library(ggplot2)
  
	if (!getwd() == pwd) {
		setwd(pwd)
	}
	
  ##Check if data is supplied, otherwise get it
  if (is.null(NEI)) {
    fetchSourceData()
  	NEI <- readSourceData()
  }
  ##Create a subset for Baltimore City, Maryland
  NEI_BM <- NEI[which(NEI$fips == "24510"), ]
  ##Summarize the PM2.5 emissions by year and type
  tbyt_BM <- aggregate(NEI_BM$Emissions, by=list(Year=NEI_BM$year, Type=NEI_BM$type), FUN=sum, na.rm=TRUE)
  ##Initialize the PNG device
  if (pPNG == TRUE) {
    png(filename = "plot3.png",
        width = 1920, height = 480, 
        units = "px", pointsize = 12,
        bg = "white")
  }
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
  return(NEI)
}