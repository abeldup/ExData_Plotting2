#######################################################################################################################
## Function makePlot4 performs the following 
##  - Calls the function to download and unzip the data
##  - Calls the function to read the data 
##  - Creates a subset of data for the Combustion of Coal
##  - Summarizes the subset by year
##  - Activates the PNG output device and generates the plot
##  - Closes the PNG output device
## Parameters: pwd = the working directory
##             pPNG = generate the PNG output
## Returns: NEI (so that it can be re-used)
#######################################################################################################################

makePlot4 <- function(pwd, pPNG = TRUE) {
  
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
  if (is.null(SCC)) {
    SCC <- readRDS("Source_Classification_Code.rds")
  }
  ##Create a subset of SCC values for Combustion of Coal
  comb <- grepl('Combustion', SCC$SCC.Level.One, fixed=TRUE)
  coal <- grepl('Coal', SCC$SCC.Level.Three, fixed=TRUE)
  SCC_CC <- subset(SCC, comb & coal, c("SCC","Short.Name","SCC.Level.One","SCC.Level.Three"))
  ##Create a subset of NEI measurements for Combustion of Coal
  NEI_CC <- NEI[which(NEI$SCC %in% SCC_CC$SCC), ]
  ##Summarize the PM2.5 emissions caused by coal combustion, by year
  tCCby <- aggregate(NEI_CC$Emissions, by=list(Year=NEI_CC$year), FUN=sum, na.rm=TRUE)
  ##Calculate a smooting spline
  SS4 <- smooth.spline(tCCby$Year, tCCby$x, spar=0.35)
  ##Initialize the PNG device
  if (pPNG == TRUE) {
    png(filename = "plot4.png",
        width = 480, height = 480, 
        units = "px", pointsize = 12,
        bg = "white")
  }
  ##Now plot the results
  plot(tCCby, type="l", col="Red", main="Total Coal Combustion Emissions", 
       ylab="PM2.5 Emissions (tons)", xlab="Year", xlim=c(1998, 2010))
  points(tCCby, pch=10)
  lines(SS4, col="Black", lty="dotdash")
  legend("topright", c("PM2.5","Trend"), lty=c(1,5), col=c("Red","Black"))
  ##Close the PNG device
  if (pPNG == TRUE) {
    dev.off()  
  }
  return(NEI)
}