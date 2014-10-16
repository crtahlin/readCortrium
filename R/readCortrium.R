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
#' @param recordType The type of recorded signal to read - 
#' "raw" (data only high-pass filtered at 0.3Hz) or 
#' "filtered" (data further low-passs filtered at 40Hz). Defaults to "raw".
#' @export
readECG <- function (dirname, recordType="raw") {
  # for each ECG lead
  for (i in 1:3) {
    if (recordType=="raw") {filename <- paste0(dirname, "ecg", i, "_raw.bin")}
    if (recordType=="filtered") {filename <- paste0(dirname, "ecg", i, "_filt.bin")}  
    
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

#' @title A simple function to extract heart beat timings from ECG data
#' 
#' @description Reads the ECG signal data and applies a highpass filter to 
#' exagurate the spikes of the QRS complex. Than looks where the spikes are higher
#' than some value and assumes that represents the R part of the ECG. Looks
#' where the next peak is more than 25 values (100ms) away from the previous one
#' and assumes this as the start of the next heart beat. Uses the
#' ECG1 records. This is a simple method - better ones exist - 
#' for a quick implementation.
#' Outputs data frame with columns representing beat times in seconds. 
#' 
#' @param dirname The path to the directory containing the ecgX_raw.bin files.
#' @export
beatsFromECG <- function (dirname, cutoffValue=200, highpassFreq=0.3) {
  # read the ECG values
  ECG <- readECG(dirname, recordType = "raw")
  # load library for manipulating signals
  library(signal)
  # apply high pass filter to the data
  highpass <- butter(2, highpassFreq, type="high")
  ECGfiltered <- filter(highpass, x=ECG$ECG1)
  
  # extract high values
  highValues <- which(ECGfiltered>cutoffValue)
  # we do 250Hz sampling and the QRS complex takes up to 0.1s 
  # (http://en.wikipedia.org/wiki/Electrocardiography)
  # so let's combine the values within this window into one value
  # 0.1s = 100ms = 25 consecutive measurements
  isBeat <- vector()
  for (i in 2:length(highValues)) {
    # the first high value is presumably a "heart beat"
    isBeat[1] <- TRUE
    # the following high values are compared to the previous value
    # browser()
    isBeat[i] <- highValues[i] > (highValues[i-1] + 25)
  }
  
  # calcluate beat times for records marked as new beats
  beatTimes <- highValues[isBeat==TRUE]/250
  
  # return a data frame of beat times in seconds
  return(as.data.frame(beatTimes))
  
}