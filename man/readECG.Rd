% Generated by roxygen2 (4.0.1): do not edit by hand
\name{readECG}
\alias{readECG}
\title{Read ECG records}
\usage{
readECG(dirname, recordType = "raw")
}
\arguments{
\item{dirname}{The path to the directory containing the ecgX_raw.bin files
(assumes bat_adc.txt is in the same directory).}

\item{recordType}{The type of recorded signal to read -
"raw" (data only high-pass filtered at 0.3Hz) or
"filtered" (data further low-passs filtered at 40Hz). Defaults to "raw".}
}
\description{
Read the ECG records contained in the ecgX_raw.bin files and
outputs a dataframe with columns for each of the three ECG leads.
Sampled at 250 Hz.

NOTE: This is raw ECG data. Nothing is currently done with values
greater than 32676 - in the matlab scripts they seem to be set to 0.
}
\details{
Returns the start and end of measurement as an attribute measurementStart and
measurementEnd.
}

