#' @title Read body and external temperature
#' 
#' @description Reads the temp.bin file saved by the cortrium C3 and outputs
#' a dataframe with columns for body temperature and external temperature.
#' Sampled at 25 Hz. 
#' 
#' @details Returns the start of measurement as an attribute measurementStart
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
  results <- data.frame(degreesBody=degreesBody, degreesExternal=degreesExternal)
  measurementStart <- getStartofMeasurement(dirname)
  measurementEnd <- measurementStart + (length(degreesAll)/2)/(25) 
      # two measurements at each occasion, 25 Hz frequency of measurement
  attr(results, which="measurementStart") <- measurementStart
  attr(results, which="measurementEnd") <- measurementEnd
  # calculate end of recording time
  
  
  # return data frame with results
  return(results)  
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
#' @param dirname The path to the directory containing the ecgX_raw.bin files 
#' (assumes bat_adc.txt is in the same directory).
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
#' @param dirname The path to the directory containing the resp_raw.bin files
#' (assumes bat_adc.txt is in the same directory).
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
#' @param dirname The path to the directory containing the ecgX_raw.bin files
#' (assumes bat_adc.txt is in the same directory).
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


#' @title Function that adds a timestamp to a data frame
#' 
#' @description Takes a data frame (that has the starting value of measurements as an attribute) 
#' and sampling frequency as input. Outputs the data frame with additional column containing
#' timestamps of measurement.
#' 
#' @param dataframe The name of the dataframe.
#' @param frequency The frequency of measurement in Hz. 
#' @param start The initial timestamp.
#' 
#' @export
addTimestamp <- function (dataframe, frequency, start=attr(dataframe, which="measurementStart")) {
  
  length <- dim(dataframe)[1]
  timestamp <- vector()
  class(timestamp) <- "POSIXct"
 
  X=(1:length)
  # timestamp is the intial timestamp + number of whole seconds
  makeTimestamp <- function(x, frequency) {start + x %/% frequency}
  values <- vector()
  class(values) <- "POSIXct"
  result <- lapply(X, FUN=makeTimestamp, frequency=frequency)
  # when "unlisted", the POSIXct class gets lost, so here I set it again
  # surely, there must be a better way?
  timestamp <- as.POSIXct(unlist(result), origin="1970-01-01")
  
  # return the data frame with added timestamp
  data <- data.frame(dataframe, timestamp=timestamp)
  return(data)  
}