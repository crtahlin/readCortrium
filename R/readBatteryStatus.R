#' @title Read battery status data
#' 
#' @description Read the records about battery status contained in the bat_adc.txt
#' file and output a data frame with column containing the timestamp,
#'  ??? and battery status.
#' 
#' @param dirname The path to the directory containing the bat_adc.txt files.
#' 
#' @export
readBatteryStatus <- function (dirname) {
  
  filename <- paste0(dirname,"bat_adc.txt")
  # import data and name columns
  # TODO: what does the 2nd column represent, battery voltage?
  data <- read.csv2(filename, header=FALSE, col.names=c("timestamp","unknownVar","status"))
  # make the first column a POSIXct date
  # assume a default "local" timezone
  data[,"timestamp"] <- as.POSIXct(data[,"timestamp"], format="%Y-%m-%d %H.%M.%S")
  
  # return battery status data
  return(data)  
}