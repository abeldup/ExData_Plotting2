#######################################################################################################################
## Function makePlot3 performs the following 
##  - Calls the function to download and unzip the data
##  - Calls the function to read the data 
##  - Creates a subset of data for Baltimore, Maryland
##  - Activates the PNG output device and generates the plot
##  - Closes the PNG output device
## Parameters: pPNG = generate the PNG output
#######################################################################################################################

makePlot3 <- function(pPNG = TRUE) {
  
  library(ggplot2)
  
  setwd("~/Documents/GitHub/datasciencecoursera/ExData_Plotting2")
  
  ##Check if data is supplied, otherwise get it
  if (is.null(NEI)) {
    fetchSourceData()
    readSourceData()
  }
  ##Initialize the PNG device
  if (pPNG == TRUE) {
    png(filename = "plot3.png",
        width = 1920, height = 480, 
        units = "px", pointsize = 12,
        bg = "white")
  }
  ##Create a subset for Baltimore City, Maryland
  if (is.null(NEI_BM)) {
    NEI_BM <- NEI[which(NEI$fips == "24510"), ]
  }
  ##Summarize the PM2.5 emissions by year and type
  if (is.null(tbyt_BM)) {
    tbyt_BM <- aggregate(NEI_BM$Emissions, by=list(Year=NEI_BM$year, Type=NEI_BM$type), FUN=sum, na.rm=TRUE)
  }
  ##Now plot the results
  qplot(y=x, x=Year, data=tbyt_BM, facets=.~Type, geom=c("point", "smooth"), method="lm")
  ##Close the PNG device
  if (pPNG == TRUE) {
    dev.off()  
  }
}