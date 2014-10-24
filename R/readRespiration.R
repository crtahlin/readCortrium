#' @title Read respiration rate records
#' 
#' @description Read the respiration rate records contained in the resp_raw.bin 
#' file and output them in a data frame. Sampled at 250 Hz.
#' 
#' NOTE: This is raw respiration rate data. It has to be further processed to 
#' get a sensible respiration rate (local maxima imply a maximum chest expansion?).
#' 
#' @param dirname The path to the directory containing the resp_raw.bin files
#' (assumes bat_adc.txt is in the same directory).
#' 
#' @details Returns the start and end of measurement as an attribute measurementStart and 
#' measurementEnd.
#' 
#' @export
readRespiration <- function (dirname) {
  filename <- paste0(dirname,"resp_raw.bin")
  fileSize <- file.info(filename)$size
  # open a connection to the file
  con <- file(filename, open="rb")
  # read respiration rate
  respRate <- readBin(con=con, what=integer(), n=fileSize/2, size=2, signed=TRUE)
  
  close(con)
  
  # add data about start of recording as an attribute
  results <- data.frame(respRate)
  measurementStart <- getStartofMeasurement(dirname)
  measurementEnd <- measurementStart + (dim(results)[1])/(250) 
  # 250 Hz frequency of measurement
  attr(results, which="measurementStart") <- measurementStart
  attr(results, which="measurementEnd") <- measurementEnd
  
  # return respiration rate records
  return(results)
}