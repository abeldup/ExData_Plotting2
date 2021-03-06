#######################################################################################################################
## Function readSourceData performs the following 
## - Reads the PM2.5 emissions data into data frame NEI
## - Reads the source classification code lookup table
## Parameters: NA
## Returns: NEI
#######################################################################################################################

readSourceData <- function() {
  
  ## This first line will likely take a few seconds. Be patient!
  NEI <- readRDS("summarySCC_PM25.rds")
  
  return(NEI)
}