#' @title A SIMPLE!!! function to extract heart beat timings from ECG data
#' 
#' @description Reads the ECG signal data and applies a highpass filter to 
#' exagurate the spikes of the QRS complex. Than looks where the spikes are higher
#' than some value and assumes that represents the R part of the ECG. Looks
#' where the next peak is more than 25 values (100ms) away from the previous one
#' and assumes this as the start of the next heart beat. Uses the
#' ECG1 records. This is a simple method - better ones exist - 
#' for a quick implementation.
#' Outputs data frame with columns representing beat times in seconds,
#'  beat timestamps and momentary heart beat values. 
#' 
#' @param dirname The path to the directory containing the ecgX_raw.bin files
#' (assumes bat_adc.txt is in the same directory).
#' 
#' @details Returns the start of measurement as an attribute measurementStart.
#' 
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
  results <- as.data.frame(beatTimes)
  
  # add data about start of recording as an attribute
  measurementStart <- getStartofMeasurement(dirname) 
  attr(results, which="measurementStart") <- measurementStart
  
  # add timestamps of each beat time to the data frame
  results[,"timestamp"] <- measurementStart + results[, "beatTimes"]
  
  # add momentary HR column
  # calculated as inverted difference between times of beats and 
  # value assigned to time of next beat (perhaps better if assigned
  # to the middle of the interval?)
  results[-1,"momentaryHR"] <- 60* (1/diff(results[,"beatTimes"]))
  
  # return a data frame of beat times in seconds
  return(results)
  
}