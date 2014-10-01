# read degrees od Celsius
con <- file("/home/crtah/Dropbox/DataSets/Cortrium/Data/2014-09-16 16.07.25/temp.bin", open = "rb")
degreesAll <- readBin(con=con, what=integer(), n=500, size=2)*0.02 - 273.15
head(degreesAll)
# two kinds of temperatures are measured - external and body temperature
# Measurements are made in pairs
# separate measurements into degreesBody and degreesExternal
length(degreesAll)
indicesDegreesExternal <- seq(from=1, to=length(degreesAll), by=2)
degreesExternal <- degreesAll[indicesDegreesExternal]
indicesDegreesBody <- seq(from=2, to=length(degreesAll), by=2)
degreesBody <- degreesAll[indicesDegreesBody]


# read respiration rate
# this is a bit complicated - local maxima have to be determined for saying "this is breath-in"
fileSize <- file.info("/home/crtah/Dropbox/DataSets/Cortrium/Data/2014-09-16 16.07.25/resp_raw.bin")$size
con <- file("/home/crtah/Dropbox/DataSets/Cortrium/Data/2014-09-16 16.07.25/resp_raw.bin", open = "rb")
temp <- readBin(con=con, what=integer(), n=fileSize/2, size=2, signed=TRUE)
hist(temp)
head(temp)

# read ECG data
con <- file("/home/crtah/Dropbox/DataSets/Cortrium/Data/2014-09-16 16.07.25/ecg1_raw.bin", open = "rb")
fileSize <- file.info("/home/crtah/Dropbox/DataSets/Cortrium/Data/2014-09-16 16.07.25/ecg1_raw.bin")$size
temp <- readBin(con=con, what=integer(), n=fileSize, size=2, signed=TRUE)
# convert to time series, sampling frequency is 250 Hz
temp <- ts(temp, frequency = 250)
temp
hist(temp)
tail(temp)
plot(temp)
