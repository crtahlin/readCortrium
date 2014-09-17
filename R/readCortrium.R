# read degrees od Celsius
con <- file("/home/crtah/Dropbox/DataSets/Cortrium/Data/2014-09-16 16.07.25/temp.bin", open = "rb")
readBin(con=con, what=integer(), n=500, size=2)*0.02 - 273.15

# read respiration rate
fileSize <- file.info("/home/crtah/Dropbox/DataSets/Cortrium/Data/2014-09-16 16.07.25/resp_raw.bin")$size
con <- file("/home/crtah/Dropbox/DataSets/Cortrium/Data/2014-09-16 16.07.25/resp_raw.bin", open = "rb")
temp <- readBin(con=con, what=integer(), n=fileSize/2, size=2, signed=TRUE)
# seems to be RAW in the sense, that the numbers make no sense?
hist(temp)
