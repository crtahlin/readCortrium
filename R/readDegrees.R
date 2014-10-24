#' @title Read body and external temperature
#' 
#' @description Reads the temp.bin file saved by the cortrium C3 and outputs
#' a dataframe with columns for body temperature and external temperature.
#' Sampled at 25 Hz. 
#' 
#' @details Returns the start and end of measurement as an attribute measurementStart and 
#' measurementEnd.
#' 
#' @param dirname The path to the directory containing the temp.bin file 
#' (assumes bat_adc.txt is in the same directory).
#' @export
readDegrees <- function (dirname) {
  filename <- paste0(dirname,"temp.bin")
  fileSize <- file.info(filename)$size
  # open a connection to the file
  con <- file(filename, open="rb")
  # read degrees and convert them from Kelvin to Celsuis scale
  degreesAll <- readBin(con=con, what=integer(), n=fileSize/2, size=2)*0.02 - 273.15
  
  # Two kinds of temperatures are measured - external and body temperature,
  # and they are made in pairs.
  # separate measurements into degreesBody and degreesExternal
  length(degreesAll)
  indicesDegreesExternal <- seq(from=1, to=length(degreesAll), by=2)
  degreesExternal <- degreesAll[indicesDegreesExternal]
  indicesDegreesBody <- seq(from=2, to=length(degreesAll), by=2)
  degreesBody <- degreesAll[indicesDegreesBody]
  close(con)
  
  # add data about start of recording as an attribute
  results <- data.frame(degreesTarget=degreesBody, degreesAmbient=degreesExternal)
  measurementStart <- getStartofMeasurement(dirname)
  measurementEnd <- measurementStart + (length(degreesAll)/2)/(25) 
  # two measurements at each occasion, 25 Hz frequency of measurement
  attr(results, which="measurementStart") <- measurementStart
  attr(results, which="measurementEnd") <- measurementEnd
  
  # return data frame with results
  return(results)  
}