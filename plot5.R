#######################################################################################################################
## Function makePlot5 performs the following 
##  - Calls the function to download and unzip the data
##  - Calls the function to read the data 
##  - Creates a subset of data for vehicle transport, for Baltimore, Maryland
##  - Summarizes the subset by year
##  - Activates the PNG output device and generates the plot
##  - Closes the PNG output device
## Parameters: pwd = the working directory
##             pPNG = generate the PNG output
#######################################################################################################################

makePlot5 <- function(pwd, pPNG = TRUE) {
  
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
  ##Create a subset of SCC values for vehicular transport
  veh <- grepl("Vehicle|Transportation|highway", SCC$SCC.Level.Two, fixed=FALSE)
  SCC_veh <- subset(SCC, veh, c("SCC","Short.Name","SCC.Level.One","SCC.Level.Two"))
  ##Create a subset of NEI measurements for vehicular transport, for Baltimore, Maryland
  NEI_vehBM <- NEI[which((NEI$SCC %in% SCC_veh$SCC) & 
                         (NEI$fips == "24510")), ]
  ##Summarize the PM2.5 emissions by year
  tby_vehBM <- aggregate(NEI_vehBM$Emissions, by=list(Year=NEI_vehBM$year), FUN=sum, na.rm=TRUE)
  ##Calculate a smooting spline
  SS5 <- smooth.spline(tby_vehBM$Year, tby_vehBM$x, spar=0.35)
  ##Initialize the PNG device
  if (pPNG == TRUE) {
    png(filename = "plot5.png",
        width = 480, height = 480, 
        units = "px", pointsize = 12,
        bg = "white")
  }
  ##Now plot the results
  plot(tby_vehBM, type="l", col="Blue", main="Total Vehicular Emissions for Baltimore, Maryland", 
       ylab="PM2.5 Emissions (tons)", xlab="Year", xlim=c(1998, 2010))
  points(tby_vehBM, pch=10)
  lines(SS5, col="Black", lty="dotdash")
  legend("topright", c("PM2.5","Trend"), lty=c(1,5), col=c("Blue","Black"))
  ##Close the PNG device
  if (pPNG == TRUE) {
    dev.off()  
  }
  return(NEI)
}