#######################################################################################################################
## Function fetchSourceData performs the following 
##  - Checks if the zipped data file exists
##  - Downloads the zipped data file 
##  - Unzips the data file
## Parameters: NA
#######################################################################################################################

fetchSourceData <- function() {
  
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  zipfile <- "exdata-data-NEI_data.zip"
  if(!file.exists(zipfile)) {
    download.file(fileURL, destfile = zipfile, method="curl")
    unzip(zipfile)
  }
  
}
