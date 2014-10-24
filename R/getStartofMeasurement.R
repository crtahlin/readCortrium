#' @title Read date and time of the start of measurement
#' 
#' @description Uses the function readBatteryStatus() to get data and 
#' extracts and returns the timestamp of the first entry, assuming this is the
#'  start of all the measurements.
#' 
#'  @param  dirname The path to the directory containing the bat_adc.txt files.
#'  
#'  @export
getStartofMeasurement <- function (dirname) {
  # get data about battery status
  data <- readBatteryStatus(dirname)
  # extract the first timestamp
  start <- data[1, "timestamp"]
  
  # return the timestamp of first measurement
  return(start)  
}