#' @title Read body and external temperature
#' 
#' @description Reads the temp.bin file saved by the cortrium C3 and outputs
#' a dataframe with columns for body temperature and external temperature.
#' Sampled at 25 Hz.
#' 
#' @param dirname The path to the directory containing the temp.bin file.
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
  # return data frame with results
  return(data.frame(degreesBody, degreesExternal))  
}


#' @title Read ECG records
#' 
#' @description Read the ECG records contained in the ecgX_raw.bin files and 
#' outputs a dataframe with columns for each of the three ECG leads. 
#' Sampled at 250 Hz.
#' 
#' NOTE: This is raw ECG data. Nothing is currently done with values 
#' greater than 32676 - in the matlab scripts they seem to be set to 0.
#' 
#' @param dirname The path to the directory containing the ecgX_raw.bin files.
#' @export
readECG <- function (dirname) {
  # for each ECG lead
  for (i in 1:3) {
    filename <- paste0(dirname, "ecg", i, "_raw.bin")
    fileSize <- file.info(filename)$size
    # open a connection to the file
    con <- file(filename, open="rb")
    
    # read the records
    assign( x=paste0("ECG", i), value=readBin(con=con, what=integer(), n=fileSize/2, size=2, signed=TRUE) )
    
    close(con)
  }
  
  # return ECG records
  return(data.frame(ECG1, ECG2, ECG3))
  
}

#' @title Read respiration rate records
#' 
#' @description Read the respiration rate records contained in the resp_raw.bin 
#' file and output them in a data frame. Sampled at 250 Hz.
#' 
#' NOTE: This is raw respiration rate data. It has to be further processed to 
#' get a sensible respiration rate (local maxima imply a maximum chest expansion?).
#' 
#' @param dirname The path to the directory containing the resp_raw.bin files.
#' @export
readRespiration <- function (dirname) {
  filename <- paste0(dirname,"resp_raw.bin")
  fileSize <- file.info(filename)$size
  # open a connection to the file
  con <- file(filename, open="rb")
  # read respiration rate
  respRate <- readBin(con=con, what=integer(), n=fileSize/2, size=2, signed=TRUE)
  
  close(con)
  # return respiration rate records
  return(data.frame(respRate))
}