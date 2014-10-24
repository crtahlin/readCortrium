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