#' @title Read acceleremeter data
#' 
#' @description Reads the accel.bin file saved by the cortrium C3 and outputs
#' a dataframe with columns for each of the three accelerometer readings.
#' Sampled at 25 Hz. 
#' 
#' @details Returns the start and end of measurement as an attribute measurementStart and 
#' measurementEnd.
#' 
#' @param dirname The path to the directory containing the accel.bin file 
#' (assumes bat_adc.txt is in the same directory).
#' @export
readAccel <- function (dirname) {
  filename <- paste0(dirname,"accel.bin")
  fileSize <- file.info(filename)$size
  # open a connection to the file
  con <- file(filename, open="rb")
  # read accelerometer reading for all three axis
  accelReadingsAll <- readBin(con=con, what=double(), n=fileSize, size=4)
  
  # Three kinds of data are measured - the x, y and z axis,
  # and they are made in triplets.
  # separate measurements into x,y,z axis
  lengthAll <- length(accelReadingsAll)
  indicesX <- seq(from=1, to=lengthAll, by=3)
  indicesY <- seq(from=2, to=lengthAll, by=3)
  indicesZ <- seq(from=3, to=lengthAll, by=3)
  accelX <- accelReadingsAll[indicesX]
  accelY <- accelReadingsAll[indicesY]
  accelZ <- accelReadingsAll[indicesZ]
  close(con)
  
  # add data about start of recording as an attribute
  results <- data.frame(accelX=accelX, accelY=accelY, accelZ=accelZ)
  measurementStart <- getStartofMeasurement(dirname)
  measurementEnd <- measurementStart + (length(accelReadingsAll)/3)/(25) 
  # two measurements at each occasion, 25 Hz frequency of measurement
  attr(results, which="measurementStart") <- measurementStart
  attr(results, which="measurementEnd") <- measurementEnd
  
  # return data frame with results
  return(results)  
  
}