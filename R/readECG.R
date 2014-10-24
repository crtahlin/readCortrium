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
#' 
#' @details Returns the start and end of measurement as an attribute measurementStart and 
#' measurementEnd.
#' 
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
  
  # add data about start of recording as an attribute
  results <- data.frame(ECG1, ECG2, ECG3)
  measurementStart <- getStartofMeasurement(dirname)
  measurementEnd <- measurementStart + (dim(results)[1])/(250) 
  # 250 Hz frequency of measurement
  attr(results, which="measurementStart") <- measurementStart
  attr(results, which="measurementEnd") <- measurementEnd
  
  # return ECG records
  return(results)
  
}