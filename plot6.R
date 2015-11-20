#######################################################################################################################
## Function makePlot6 performs the following 
##  - Calls the function to download and unzip the data
##  - Calls the function to read the data 
##  - Creates a subset of data for vehicle transport, for Baltimore, Maryland and Los Angeles County, California
##  - Summarizes the subset by year
##  - Activates the PNG output device and generates the plot
##  - Closes the PNG output device
## Parameters: pwd = the working directory
##             pPNG = generate the PNG output
## Returns: NEI (so that it can be re-used)
#######################################################################################################################

makePlot6 <- function(pwd, pPNG = TRUE) {
  
  library(ggplot2)
  
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
  ##Create a subset of NEI measurements for vehicular transport, 
  ##for Baltimore, Maryland and Los Angeles County, California
  NEI_vehBMLA <- NEI[which((NEI$SCC %in% SCC_veh$SCC) & 
                           (NEI$fips%in% c("24510", "06037"))), ]
  ##Summarize the PM2.5 emissions by year
  tby_vehBMLA <- aggregate(NEI_vehBMLA$Emissions, 
                           by=list(Year=NEI_vehBMLA$year,fips=NEI_vehBMLA$fips), 
                           FUN=sum, na.rm=TRUE)
  ##Name the cities
  tby_vehBMLA$City <- ifelse(tby_vehBMLA$fips == "24510", "Baltimore", "Los Angeles")
  ##Initialize the PNG device
  if (pPNG == TRUE) {
    png(filename = "plot6.png",
        width = 720, height = 480, 
        units = "px", pointsize = 12,
        bg = "white")
  }
  ##Now plot the results
  qp = qplot(y=x, x=Year, data=tby_vehBMLA, color=City, 
             geom=c("point", "smooth"), method="lm", 
             ylab="PM2.5 Emissions (tons)",
             main="Total Vehicular Emissions for Baltimore and Los Angeles") +
    coord_cartesian(ylim = c(0, 10000))
  print(qp)
  ##Close the PNG device
  if (pPNG == TRUE) {
    dev.off()  
  }
  return(NEI)
}